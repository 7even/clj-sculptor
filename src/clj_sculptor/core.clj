(ns clj-sculptor.core
  (:require [clj-sculptor.rules :as rules]
            [rewrite-clj.zip :as z]))

(defn format-code
  "Format code using the strip-and-generate approach."
  [code-str]
  (-> code-str
      (z/of-string {:track-position? true})
      rules/normalize-collection-types
      rules/sort-ns-forms
      rules/strip-whitespace
      rules/insert-whitespace
      z/root-string))
