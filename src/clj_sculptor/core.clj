(ns clj-sculptor.core
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]))

(defn parse-string
  "Parse a Clojure string into a zipper with comments preserved."
  [s]
  (z/of-string s))

(defn format-code
  "Format code using predefined formatting rules."
  [zloc]
  (z/root-string zloc))

(defn apply-formatting-rules
  "Apply formatting rules to the parsed code."
  [zloc]
  zloc)
