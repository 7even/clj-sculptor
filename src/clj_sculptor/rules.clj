(ns clj-sculptor.rules
  "Collection of formatting rules based on the Clojure Style Guide."
  (:require [clj-sculptor.core :as core]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

;; -----------------------------------------------------------------------------
;; Whitespace rules

(defmethod core/apply-rules :whitespace [zloc]
  (let [node (z/node zloc)
        content (str node)]
    (cond
      ;; Case 1: Trailing whitespace (spaces before newline or end of file)
      (and (re-find #"^ +$" content) ; Only spaces
           (or (z/end? (z/next* zloc)) ; At end of file
               (= (z/tag (z/next* zloc)) :newline))) ; Before newline
      ;; Remove the whitespace entirely
      (z/remove* zloc)

      ;; Case 2: Indentation whitespace (spaces after newline)
      (and (re-find #"^ +$" content) ; Only spaces
           (let [prev (z/prev* zloc)]
             (and prev (= (z/tag prev) :newline))))
      ;; Calculate proper indentation based on AST depth
      (let [current-spaces (count content)
            target-spaces (core/calculate-indentation-depth zloc)]
        (cond
          (= target-spaces 0)
          ;; Remove indentation entirely for top-level forms
          (z/remove* zloc)

          (not= current-spaces target-spaces)
          ;; Replace with correct number of spaces
          (z/replace zloc (n/spaces target-spaces))

          :else
          zloc))

      ;; Case 3: Embedded trailing whitespace
      (re-find #" +\n" content)
      (let [cleaned (core/remove-trailing-whitespace content)]
        (z/replace zloc (n/whitespace-node cleaned)))

      ;; Case 4: No changes needed
      :else
      zloc)))

(defmethod core/apply-rules :newline [zloc]
  ;; Newlines are separate from whitespace in rewrite-clj
  ;; For now, just return as-is
  zloc)

;; -----------------------------------------------------------------------------
;; Comma rules

(defmethod core/apply-rules :comma [zloc]
  ;; Commas should be removed from sequential collections
  ;; But this needs context about parent node
  ;; For now, just return as-is
  zloc)

;; -----------------------------------------------------------------------------
;; List rules (function calls, special forms)

(defmethod core/apply-rules :list [zloc]
  ;; Will handle:
  ;; - Function argument alignment
  ;; - Special form formatting (let, cond, etc.)
  zloc)

;; -----------------------------------------------------------------------------
;; Vector rules

(defmethod core/apply-rules :vector [zloc]
  ;; Will handle:
  ;; - Destructuring alignment
  ;; - Vector literal formatting
  zloc)

;; -----------------------------------------------------------------------------
;; Map rules

(defmethod core/apply-rules :map [zloc]
  ;; Will handle:
  ;; - Key-value alignment
  ;; - Map literal formatting
  zloc)
