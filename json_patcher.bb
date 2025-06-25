#!/usr/bin/env bb

(ns json-patcher
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [babashka.http-client :as http]
            [babashka.process :as p]
            [shell-smith.core :as smith]))

(def version "0.1.0")

(def usage "json-patcher - The chief cook and bottle washer of json patching

Processes JSON file versions and generates patches, fixes, and changelogs.

Usage:
  json-patcher process <from-version> <to-version> [options]
  json-patcher --help
  json-patcher --version

Arguments:
  <from-version>    Source version (e.g., v1, v2)
  <to-version>      Target version (e.g., v2, v3)

Options:
  --base-dir=<base-dir>              Base directory containing JSON files [default: .]
  --llm-endpoint=<llm-endpoint-url>  LLM API endpoint for human-readable conversion
  --llm-api-key=<llm-api-key>        API key for LLM service
  --llm-model=<llm-model>            Model for LLM service [default: gpt-4.1]
  --webhook-url=<webhook-url>        Webhook URL for notifications
  --webhook-token=<webhook-token>    Authentication token for webhook
  --changelog=<changelog>            Changelog output file [default: CHANGELOG.md]
  --dry-run                          Show what would be done without executing
  --verbose                          Enable verbose output
  --help                             Show this help message
  --version                          Show version information

Examples:
  json-patcher process v1 v2 --llm-endpoint=https://api.openai.com/v1/chat/completions
  json-patcher process v2 v3 --webhook-url=https://hooks.slack.com/... --dry-run
  json-patcher process v1 v2 --base-dir=/path/to/files --verbose

Environment Variables:
  JSON_PATCHER_LLM_ENDPOINT    Default LLM API endpoint
  JSON_PATCHER_LLM_API_KEY     Default LLM API key
  JSON_PATCHER_LLM_MODEL       Default LLM Model
  JSON_PATCHER_WEBHOOK_URL     Default webhook URL
  JSON_PATCHER_WEBHOOK_TOKEN   Default webhook token")

(defn log [verbose & args]
  (when verbose
    (apply println "[LOG]" args)))

(defn error [& args]
  (apply println "[ERROR]" args))

(defn find-files-for-version
  "Find JSON files for a specific version"
  [base-dir version]
  (let [pattern (re-pattern (str "\\d{8}T\\d{6}Z-" version "\\.json$"))
        fix-pattern (re-pattern (str "\\d{8}T\\d{6}Z-" version "-fix\\.json$"))
        files (file-seq (io/file base-dir))
        json-files (filter #(and (.isFile %) (re-find pattern (.getName %))) files)
        fix-files (filter #(and (.isFile %) (re-find fix-pattern (.getName %))) files)]
    {:regular (first json-files)
     :fix (first fix-files)}))

(defn run-command
  "Execute a shell command"
  [cmd dry-run verbose]
  (log verbose "Running:" cmd)
  (if dry-run
    (println "[DRY-RUN]" cmd)
    (let [result (p/shell {:out :string :err :string :continue true} cmd)]
      ;; (when (not= 0 (:exit result))
      ;;   (throw (ex-info (str "Command failed: " cmd)
      ;;                   {:exit (:exit result)
      ;;                    :stderr (:err result)})))
      result)))

(defn generate-changes-patch
  "Generate patch between two versions"
  [from-file to-file output-file dry-run verbose]
  (let [cmd (str "jsondiff " (.getPath from-file) " " (.getPath to-file))
        result (:out (run-command cmd dry-run verbose))]
    ;; (prn result)
    (spit output-file result)
    output-file))

(defn generate-fix-patch
  "Generate patch between regular and fix version"
  [regular-file fix-file output-file dry-run verbose]
  (let [cmd (str "jsondiff " (.getPath regular-file) " " (.getPath fix-file))
        result (:out (run-command cmd dry-run verbose))]
    (spit output-file result)
    output-file))

(defn apply-fix-patch
  "Apply fix patch to target file"
  [target-file patch-file output-file dry-run verbose]
  (let [cmd (str "jsonpatch --indent 2 " (.getPath target-file) " " patch-file)
        result (:out (run-command cmd dry-run verbose))]
    (spit output-file result)
    output-file))

(defn convert-to-human-readable
  "Convert JSON patch to human-readable text using LLM"
  [{:keys [llm-endpoint llm-api-key llm-model dry-run verbose]
    :as options} patch-file]
  (if dry-run
    (do
      (println "[DRY-RUN] Would convert patch to human-readable text")
      "Mock human-readable changes for dry run")
    (try
      (let [patch-content (slurp patch-file)
            prompt (str "Convert this JSON patch to human-readable changelog text. "
                        "Focus on what changed, added, or removed."
                        "Format in Slack-compatible mrkdwn."
                        "This is differen from Markdown!"
                        "Here are the basics: *bold*, _italics_, `monospaced`"
                        "Be concise but informative.:\n\n"
                        patch-content)
            payload {:model llm-model
                     :messages [{:role "user" :content prompt}]}
            response (http/post llm-endpoint
                                {:headers {"Authorization" (str "Bearer " llm-api-key)
                                           "Content-Type" "application/json"}
                                 :body (json/write-str payload)})]
        (log verbose "LLM Response status:" (:status response))
        (if (= 200 (:status response))
          (-> response :body (json/read-str :key-fn keyword)
              :choices first :message :content str/trim)
          (throw (ex-info "LLM API call failed" {:status (:status response)
                                                 :body (:body response)}))))
      (catch Exception e
        (error "Failed to convert patch to human-readable:" (.getMessage e) e)
        (str "Failed to generate human-readable text: " (.getMessage e))))))

(defn post-to-webhook
  "Post data to webhook"
  [webhook-url webhook-token data dry-run verbose]
  (if dry-run
    (println "[DRY-RUN] Would post to webhook:" (pr-str data))
    (try
      (let [headers (cond-> {"Content-Type" "application/json"}
                      webhook-token (assoc "Authorization" (str "Bearer " webhook-token)))
            response (http/post webhook-url
                                {:headers headers
                                 :body (json/write-str data)})]
        (log verbose "Webhook response status:" (:status response))
        (when (not= 200 (:status response))
          (error "Webhook post failed:" (:status response) (:body response))))
      (catch Exception e
        (error "Failed to post to webhook:" (.getMessage e))))))

;; TODO: fix this
(defn update-changelog
  "Update changelog file with new changes"
  [changelog-file changes-text version dry-run verbose]
  (if dry-run
    (println "[DRY-RUN] Would update changelog with changes for" version)
    (try
      (let [existing-content (if (.exists (io/file changelog-file))
                               (slurp changelog-file)
                               "# Changelog\n\n")
            timestamp (java.time.LocalDateTime/now)
            new-entry (str "## " version " - " timestamp "\n\n" changes-text "\n\n")
            updated-content (str/replace existing-content
                                         "# Changelog\n\n"
                                         (str "# Changelog\n\n" new-entry))]
        (spit changelog-file updated-content)
        (log verbose "Updated changelog:" changelog-file))
      (catch Exception e
        (error "Failed to update changelog:" (.getMessage e))))))

(defn generate-text [& {:keys [version human-readable json-patch] :as opts}]
  (str/join "\n\n"
            [(str "New Version: " version)
             human-readable
             (str "*Patch*\n```\n"
                  (with-out-str (json/pprint-json (json/read-str json-patch)))
                  "\n```")]))

;;; steps

(defn step-find-files
  [{:keys [base-dir from-version to-version verbose] :as options}]
  (let [from-files (find-files-for-version base-dir from-version)
        to-files (find-files-for-version base-dir to-version)]

    (when (or (nil? (:regular from-files)) (nil? (:regular to-files)))
      (throw (ex-info "Required files not found"
                      {:from-files from-files :to-files to-files})))

    (log verbose "Found files:"
         (update-vals from-files str)
         (update-vals to-files str))

    (assoc options :from-files from-files :to-files to-files)))

(defn step-generate-changes-patch
  [{:keys [from-version to-version verbose from-files to-files dry-run] :as options}]
  (let [changes-patch-file (str "patch-changes-" from-version "-" to-version ".json")]
    (log verbose "Generating changes patch...")
    (generate-changes-patch (:regular from-files) (:regular to-files)
                            changes-patch-file dry-run verbose)
    (assoc options :changes-patch-file changes-patch-file)))

(defn step-generate-fix-patch
  [{:keys [from-version from-files verbose dry-run] :as options}]
  (if (:fix from-files)
    (let [fix-patch-file (str "patch-fix-" from-version ".json")]
      (log verbose "Generating fix patch...")
      (generate-fix-patch (:regular from-files) (:fix from-files)
                          fix-patch-file dry-run verbose)
      (assoc options :fix-patch-file fix-patch-file))
    options))

(defn step-apply-fix-patch-to-newer-version
  [{:keys [fix-patch-file to-files verbose dry-run] :as options}]
  (if fix-patch-file
    (let [to-fix-file (str (.getParent (:regular to-files)) "/"
                           (str/replace (.getName (:regular to-files))
                                        ".json" "-fix.json"))]
      (log verbose "Applying fix patch...")
      (apply-fix-patch (:regular to-files) fix-patch-file
                       to-fix-file dry-run verbose)
      (assoc options :to-fix-file to-fix-file))
    options))

(defn step-convert-to-human-readable
  [{:keys [llm-endpoint to-files verbose changes-patch-file dry-run] :as options}]
  (if llm-endpoint
    (let [changes-md-file (str (.getParent (:regular to-files)) "/"
                               (str/replace (.getName (:regular to-files))
                                            ".json" "-changes.md"))
          human-readable (convert-to-human-readable options changes-patch-file)]
      (log verbose "Converting to human-readable...")
      (when-not dry-run
        (spit changes-md-file human-readable))
      (assoc options
             :changes-md-file changes-md-file
             :human-readable human-readable))
    options))

(defn step-update-changelog
  [{:keys [changelog human-readable to-version dry-run verbose] :as options}]
  (update-changelog changelog human-readable to-version dry-run verbose)
  options)

(defn step-post-to-webhook
  [{:keys [webhook-url dry-run changes-patch-file to-version human-readable webhook-token verbose] :as options}]
  (if webhook-url
    (let [patch-content (if dry-run "{}" (slurp changes-patch-file))
          webhook-text (generate-text :version to-version
                                      :human-readable human-readable
                                      :json-patch patch-content)
          webhook-data {:text webhook-text}]
      (post-to-webhook webhook-url webhook-token webhook-data
                       dry-run verbose)
      options)
    options))

;;; main

(defn process-versions
  "Main processing function"
  [{:keys [from-version to-version] :as options}]
  (let [{:keys [base-dir llm-endpoint llm-api-key webhook-url webhook-token
                changelog dry-run verbose]} options]
    (try
      (log verbose "Processing versions:" from-version "to" to-version)
      (-> options
          step-find-files
          step-generate-changes-patch
          step-generate-fix-patch
          step-apply-fix-patch-to-newer-version
          step-convert-to-human-readable
          step-update-changelog
          step-post-to-webhook)
      (log verbose "Processing completed successfully")
      (catch Exception e
        (let [message (ex-message e)]
          (error "Processing failed:" message e)
          (when (and webhook-url (not dry-run))
            (post-to-webhook webhook-url webhook-token
                             {:text (str "Error: " message)
                              :timestamp (str (java.time.Instant/now))}
                             dry-run verbose))
          (System/exit 1))))))

(defn -main [& args]
  (let [options (smith/config usage)]
    (prn options)
    (cond
      (:help options)
      (println usage)

      (:version options)
      (println "json-patcher version" version)

      (:process options)
      (process-versions options)

      :else
      (do
        (println "Invalid arguments. Use --help for usage information.")
        (System/exit 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
