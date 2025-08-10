(ns clj-sculptor.cli
  (:require [clj-sculptor.core :as core]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io])
  (:gen-class))

(def cli-options
  [["-i" "--input FILE" "Input file path"
    :validate [#(.exists (io/file %)) "Input file must exist"]]
   ["-o" "--output FILE" "Output file path (optional)"]
   ["-h" "--help" "Show help"]])

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      (println "clj-sculptor - Clojure code formatter\n" summary)

      errors
      (do
        (println "Errors:" (clojure.string/join ", " errors))
        (System/exit 1))

      (:input options)
      (let [input-content (slurp (:input options))
            parsed (core/parse-string input-content)
            formatted (-> parsed
                          core/apply-formatting-rules
                          core/format-code)]
        (if (:output options)
          (spit (:output options) formatted)
          (println formatted)))

      :else
      (println "Usage: clj-sculptor -i input.clj [-o output.clj]"))))

(defn main
  "Entry point for exec-fn"
  [opts]
  (let [args (mapcat (fn [[k v]] [(str "--" (name k)) (str v)]) opts)]
    (apply -main args)))
