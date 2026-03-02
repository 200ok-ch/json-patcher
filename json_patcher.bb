#!/usr/bin/env bb

(ns json-patcher
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [babashka.http-client :as http]
            [babashka.process :as p]
            [shell-smith.core :as smith]))

(def version "0.2.0")

(def usage "json-patcher - The chief cook and bottle washer of json patching

Usage:
  json-patcher diff <from-file> <to-file> [--out <patch-file>] [--json] [--dry-run] [--verbose]
  json-patcher describe <patch-file> [--out <md-file> | --append <changelog-file>] [--llm-endpoint <url>] [--llm-api-key <key>] [--llm-model <model>] [--json] [--dry-run] [--verbose]
  json-patcher apply <base-file> <patch-file> [--out <fixed-file>] [--json] [--dry-run] [--verbose]
  json-patcher --help
  json-patcher --version

Options:
  --out <file>                         Write output to file
  --append <changelog-file>  Append description to changelog file
  --llm-endpoint <url>                 OpenAI-compatible Chat Completions endpoint
  --llm-api-key <key>                  API key for LLM service
  --llm-model <model>                  Model for LLM service [default: gpt-4.1]
  --dry-run                            Show what would be done without executing
  --verbose                            Enable verbose output
  --json                               Output command result as JSON
  --help                               Show this help message
  --version                            Show version information

Environment Variables:
  JSON_PATCHER_LLM_ENDPOINT    Default OpenAI-compatible Chat Completions endpoint
  JSON_PATCHER_LLM_API_KEY     Default LLM API key
  JSON_PATCHER_LLM_MODEL       Default LLM model")

(defn log [verbose & args]
  (when verbose
    (apply println "[LOG]" args)))

(defn error [& args]
  (apply println "[ERROR]" args))

(defn output-json [data]
  (println (json/write-str data)))

(defn mask-secret [value]
  (if (and value (not (str/blank? value)))
    "***"
    "<unset>"))

(defn show-value [value]
  (if (and value (not (str/blank? value)))
    value
    "<unset>"))

(defn log-env [verbose]
  (when verbose
    (log true "Environment:")
    (log true (str "  JSON_PATCHER_LLM_ENDPOINT=" (show-value (System/getenv "JSON_PATCHER_LLM_ENDPOINT"))))
    (log true (str "  JSON_PATCHER_LLM_API_KEY=" (mask-secret (System/getenv "JSON_PATCHER_LLM_API_KEY"))))
    (log true (str "  JSON_PATCHER_LLM_MODEL=" (show-value (System/getenv "JSON_PATCHER_LLM_MODEL"))))))

(defn log-block [verbose title lines]
  (when verbose
    (log true (str title ":"))
    (doseq [line lines]
      (log true (str "  " line)))))

(defn now-human-utc []
  (let [formatter (java.time.format.DateTimeFormatter/ofPattern
                   "dd MMM uuuu, HH:mm 'UTC'"
                   java.util.Locale/ENGLISH)]
    (.format (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC) formatter)))

(defn ensure-file-exists [path label]
  (let [f (io/file path)]
    (when-not (.exists f)
      (throw (ex-info (str label " not found") {:file path :label label})))
    f))

(defn run-command [cmd dry-run verbose allowed-exit-codes]
  (log verbose "Running:" cmd)
  (if dry-run
    {:exit 0 :out nil :err nil :dry-run true :cmd cmd}
    (let [result (p/shell {:out :string :err :string :continue true} cmd)]
      (when-not (contains? allowed-exit-codes (:exit result))
        (throw (ex-info (str "Command failed: " cmd)
                        {:exit (:exit result)
                         :stderr (:err result)
                         :cmd cmd})))
      result)))

(defn local-description [patch-content]
  (try
    (let [patch (json/read-str patch-content :key-fn keyword)]
      (if (sequential? patch)
        (let [counts (frequencies (map :op patch))
              adds (get counts "add" 0)
              removes (get counts "remove" 0)
              replaces (get counts "replace" 0)
              moves (get counts "move" 0)
              copies (get counts "copy" 0)
              tests (get counts "test" 0)]
          (str/join "\n"
                    (remove nil?
                            ["Patch summary"
                             (when (pos? adds) (str "- Added: " adds " change(s)"))
                             (when (pos? removes) (str "- Removed: " removes " change(s)"))
                             (when (pos? replaces) (str "- Updated: " replaces " change(s)"))
                             (when (pos? moves) (str "- Moved: " moves " change(s)"))
                             (when (pos? copies) (str "- Copied: " copies " change(s)"))
                             (when (pos? tests) (str "- Tested: " tests " condition(s)"))])))
        "Patch loaded, but no list of JSON patch operations was found."))
    (catch Exception _
      "Patch loaded, but could not parse JSON patch operations.")))

(defn describe-with-llm
  [{:keys [llm-endpoint llm-api-key llm-model verbose]} patch-content]
  (let [prompt (str "Convert this JSON patch to concise, human-readable release notes. "
                    "Focus on what changed, added, or removed."
                    "Use markdown bullet points where helpful.\n\n"
                    patch-content)
        payload {:model llm-model
                 :messages [{:role "user" :content prompt}]}
        _ (log-block verbose
                     "Execution block"
                     [(str "POST " llm-endpoint)
                      (str "model=" llm-model)
                      "response path: choices[0].message.content"])
        response (http/post llm-endpoint
                            {:headers {"Authorization" (str "Bearer " llm-api-key)
                                       "Content-Type" "application/json"}
                             :body (json/write-str payload)})]
    (log verbose "LLM response status:" (:status response))
    (if (= 200 (:status response))
      (-> response :body (json/read-str :key-fn keyword)
          :choices first :message :content str/trim)
      (throw (ex-info "LLM API call failed"
                      {:status (:status response)
                       :body (:body response)})))))

(defn render-description [options patch-content]
  (if (:llm-endpoint options)
    (if (:dry-run options)
      "[DRY-RUN] Would generate human-readable description via LLM"
      (describe-with-llm options patch-content))
    (local-description patch-content)))

(defn append [changelog-file source-file text dry-run verbose]
  (let [existing-content (if (.exists (io/file changelog-file))
                           (slurp changelog-file)
                           "# Changelog\n")
        entry (str "\n## " (now-human-utc) "\n\n"
                   "Source: `" source-file "`\n\n"
                   text "\n")
        new-content (str (str/trimr existing-content) "\n" entry)]
    (if dry-run
      (log verbose "[DRY-RUN] Would append to changelog:" changelog-file)
      (do
        (spit changelog-file new-content)
        (log verbose "Appended changelog:" changelog-file)))
    changelog-file))

(defn handle-diff [options]
  (let [{:keys [from-file to-file out json dry-run verbose]} options
        from-obj (ensure-file-exists from-file "Source file")
        to-obj (ensure-file-exists to-file "Target file")
        cmd (str "jsondiff " (.getPath from-obj) " " (.getPath to-obj))
        _ (log-block verbose
                     "Execution block"
                     [cmd
                      (str "write patch: " (or out "stdout"))
                      (str "output mode: " (if json "json" "text"))])
        result (run-command cmd dry-run verbose #{0 1})
        patch-content (:out result)]
    (when (and out (not dry-run))
      (spit out patch-content)
      (log verbose "Wrote patch:" out))
    (when (and (not json) (not out) (not dry-run))
      (println patch-content))
    (when json
      (output-json {:ok true
                    :command "diff"
                    :from-file from-file
                    :to-file to-file
                    :output-file out
                    :dry-run (boolean dry-run)
                    :patch (when (and (not out) (not dry-run)) patch-content)}))))

(defn handle-describe [options]
  (let [{:keys [patch-file out json dry-run verbose]} options
        append-file (:append options)
        _ (ensure-file-exists patch-file "Patch file")
        _ (when (and out append-file)
            (throw (ex-info "--out and --append are mutually exclusive"
                            {:out out :append append-file})))
        patch-content (slurp patch-file)
        _ (log-block verbose
                     "Execution block"
                     [(str "read patch: " patch-file)
                      (if (:llm-endpoint options)
                        (str "describe via LLM endpoint: " (:llm-endpoint options))
                        "describe locally (no LLM endpoint)")
                      (cond
                        append-file (str "append changelog: " append-file)
                        out (str "write description: " out)
                        :else "write description: stdout")
                      (str "output mode: " (if json "json" "text"))])
        description (render-description options patch-content)]
    (cond
      append-file (append append-file patch-file description dry-run verbose)
      out (when-not dry-run
            (spit out description)
            (log verbose "Wrote description:" out))
      :else (when-not json
              (println description)))
    (when json
      (output-json {:ok true
                    :command "describe"
                    :patch-file patch-file
                    :output-file out
                    :changelog-file append-file
                    :dry-run (boolean dry-run)
                    :description description}))))

(defn handle-apply [options]
  (let [{:keys [base-file patch-file out json dry-run verbose]} options
        base-obj (ensure-file-exists base-file "Base file")
        _ (ensure-file-exists patch-file "Patch file")
        output-file out
        cmd (str "jsonpatch --indent 2 " (.getPath base-obj) " " patch-file)
        _ (log-block verbose
                     "Execution block"
                     [cmd
                      (str "write output: " (or output-file "stdout"))
                      (str "output mode: " (if json "json" "text"))])
        result (run-command cmd dry-run verbose #{0})
        fixed-content (:out result)]
    (when (and output-file (not dry-run))
      (spit output-file fixed-content)
      (log verbose "Wrote fixed file:" output-file))
    (when (and (not json) (not output-file) (not dry-run))
      (println fixed-content))
    (when json
      (output-json {:ok true
                    :command "apply"
                    :base-file base-file
                    :patch-file patch-file
                    :output-file output-file
                    :dry-run (boolean dry-run)
                    :content (when (and (not output-file) (not dry-run)) fixed-content)}))))

(defn -main [& _args]
  (let [options (smith/config usage :name "json-patcher")]
    (try
      (log-env (:verbose options))
      (cond
        (:help options)
        (println usage)

        (:version options)
        (println "json-patcher version" version)

        (:diff options)
        (handle-diff options)

        (:describe options)
        (handle-describe options)

        (:apply options)
        (handle-apply options)

        :else
        (do
          (println "Invalid arguments. Use --help for usage information.")
          (System/exit 1)))
      (catch Exception e
        (let [message (or (ex-message e) (.getMessage e))]
          (if (:json options)
            (output-json {:ok false
                          :error message
                          :data (ex-data e)})
            (error message))
          (System/exit 1))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
