(ns clj-sculptor.core
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]))

;; -----------------------------------------------------------------------------
;; Core parsing and formatting functions

(defn parse-string
  "Parse a Clojure string into a zipper with comments preserved."
  [s]
  (z/of-string s))

;; -----------------------------------------------------------------------------
;; Rule application via multimethods

(defmulti apply-rules
  "Apply formatting rules based on the node type.
   Dispatches on the tag of the current zipper location."
  z/tag)

;; Default implementation - no changes for unhandled node types
(defmethod apply-rules :default [zloc]
  zloc)

;; -----------------------------------------------------------------------------
;; Tree walking and rule application

(defn walk-and-format
  "Walk through the entire tree and apply rules to each node."
  [zloc]
  (loop [loc zloc]
    (if (z/end? loc)
      loc
      (let [loc' (apply-rules loc)]
        (recur (z/next* loc'))))))

(defn format-code
  "Format code by parsing, applying rules, and generating the result.
   Takes a string of Clojure code and returns formatted string."
  [code-str]
  (-> code-str
      parse-string
      walk-and-format
      z/root-string))

;; -----------------------------------------------------------------------------
;; Helper functions for rule implementations

(defn whitespace-node?
  "Check if a node is a whitespace node."
  [node]
  (n/whitespace? node))

(defn linebreak-node?
  "Check if a node is a linebreak/newline node."
  [node]
  (n/linebreak? node))

(defn comma-node?
  "Check if a node is a comma node."
  [node]
  (n/comma? node))

(defn whitespace-or-comma?
  "Check if a node is whitespace or comma."
  [node]
  (or (whitespace-node? node)
      (linebreak-node? node)
      (comma-node? node)))

(defn count-newlines
  "Count newlines in a node's string representation."
  [node]
  (count (filter #(= % \newline) (str node))))

(defn create-whitespace
  "Create a whitespace node with the specified content."
  [content]
  (n/whitespace-node content))

(defn create-newlines
  "Create a node with n newlines."
  [n]
  (n/newlines n))

(defn create-spaces
  "Create a node with n spaces."
  [n]
  (n/spaces n))

(defn remove-trailing-whitespace
  "Remove trailing whitespace from a string."
  [s]
  (str/replace s #" +(\n|$)" "$1"))

(defn split-lines-preserve-endings
  "Split a string into lines, preserving the line endings."
  [s]
  (str/split s #"(?<=\n)"))
