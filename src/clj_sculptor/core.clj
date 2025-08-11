(ns clj-sculptor.core
  (:require [clj-sculptor.rules :as rules]
            [clojure.string :as str]
            [rewrite-clj.zip :as z]))

(defn parse-string
  "Parse a Clojure string into a zipper with comments preserved."
  [s]
  (z/of-string s {:track-position? true}))

(defn walk-and-format
  "Walk through the entire tree and apply rules to each node."
  [zloc]
  (loop [loc zloc]
    (if (z/end? loc)
      loc
      (let [loc' (rules/apply-rules loc)]
        (recur (z/next* loc'))))))

(defn format-code
  "Format code by parsing, applying rules, and generating the result.
   Takes a string of Clojure code and returns formatted string."
  [code-str]
  (-> code-str
      parse-string
      walk-and-format
      z/root-string
      ;; Remove leading whitespace only from the very beginning of the document
      ;; This handles cases where the first form has embedded leading whitespace
      (str/replace #"^[ \t]+" "")))
