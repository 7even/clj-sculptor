(ns clj-sculptor.core
  (:require [clj-sculptor.rules :as rules]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

(defn format-code
  "Format code using the recursive multimethod architecture."
  [code-str]
  (let [root-node (-> code-str
                      z/of-string
                      z/root)]
    (-> (rules/format-node 0 root-node)
        n/string)))
