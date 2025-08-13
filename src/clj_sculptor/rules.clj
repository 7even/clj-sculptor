(ns clj-sculptor.rules
  "Collection of formatting rules based on the Clojure Style Guide."
  (:require [clojure.string :as str]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

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

(defn calculate-indentation-depth
  "Calculate the proper indentation depth for a zipper location.
   Returns the number of spaces that should indent this position."
  [zloc]
  ;; Context-aware indentation based on container types
  (loop [current (z/up zloc)
         depth 0]
    (if current
      (case (z/tag current)
        :forms
        ;; Stop counting when we reach :forms (document root)
        depth

        :vector
        ;; Vector contents align with first element (1 space)
        (recur (z/up current) (+ depth 1))

        :map
        ;; Map contents align with first element (1 space)
        (recur (z/up current) (+ depth 1))

        :set
        ;; Set contents align with first element (2 spaces after #{)
        (recur (z/up current) (+ depth 2))

        ;; Default case: lists and other forms use 2-space nesting
        (recur (z/up current) (+ depth 2)))
      ;; No parent, so we're at root - depth is 0
      depth)))

(defn get-parent-form-type
  "Get the type of the parent form (defn, let, if, etc.)."
  [zloc]
  (let [parent-first-elem (some-> zloc z/up z/down z/sexpr)]
    (when (symbol? parent-first-elem)
      parent-first-elem)))

(defn should-indent-as-body?
  "Check if this position should be indented as a form body (2 spaces from parent)."
  [zloc]
  (let [parent-type (get-parent-form-type zloc)]
    ;; Forms that have body indentation
    (contains? #{:defn :def :when :if :let :loop :with-out-str :cond} parent-type)))

(defn collection-parent?
  "Check if a zipper location represents a collection (map, vector, set, list)."
  [zloc]
  (contains? #{:map :vector :set :list}
             (z/tag zloc)))

(defn find-collection-parent
  "Return the parent if it's a collection, otherwise nil."
  [zloc]
  (let [parent (z/up zloc)]
    (when (collection-parent? parent)
      parent)))

(defn get-alignment-column
  "Get the column position where elements in this collection should align."
  [zloc]
  (some-> zloc
          find-collection-parent
          z/down
          z/position
          second))

(defn calculate-alignment-spaces
  "Calculate spaces needed for position-based alignment.
   Returns the number of spaces to indent this element."
  [zloc]
  ;; Use position-based alignment for collections (map, vector, set), otherwise use depth-based
  (if-let [target-col (and (some-> zloc
                                   find-collection-parent
                                   z/tag
                                   #{:map :vector :set})
                           (get-alignment-column zloc))]
    (max 0 (dec target-col)) ; target-col is 1-indexed, spaces are 0-indexed
    (calculate-indentation-depth zloc)))

;; -----------------------------------------------------------------------------
;; Whitespace rules

(defmethod apply-rules :whitespace [zloc]
  (let [node (z/node zloc)
        content (str node)]
    (cond
      ;; Leading whitespace at document start - remove entirely
      (nil? (z/prev* zloc))
      (z/remove* zloc)

      ;; Indentation whitespace - handled by :newline rule
      (let [prev (z/prev* zloc)]
        (= (z/tag prev)
           :newline))
      zloc

      ;; Trailing whitespace - remove entirely
      (or (z/end? (z/next* zloc))
          (= (z/tag (z/next* zloc))
             :newline))
      (z/remove* zloc)

      ;; Multiple spaces between tokens - normalize to single space
      (> (count content) 1)
      (z/replace* zloc (n/spaces 1))

      :else
      zloc)))

(defmethod apply-rules :newline [zloc]
  ;; Manage all indentation following this newline
  (let [next-node (z/next* zloc)]
    (cond
      ;; Skip empty lines and end of file
      (or (nil? next-node)
          (= (z/tag next-node)
             :newline))
      zloc

      ;; Handle indentation for any content after newline
      :else
      (let [target-node (if (= (z/tag next-node)
                               :whitespace)
                          (z/next* next-node) ; Skip whitespace to find content
                          next-node)
            target-spaces (calculate-alignment-spaces target-node)]
        (cond
          (= target-spaces 0)
          ;; Top-level forms need no indentation
          (if (= (z/tag next-node)
                 :whitespace)
            (z/remove* next-node)
            zloc)

          (= (z/tag next-node)
             :whitespace)
          ;; Replace existing whitespace with correct amount
          (z/replace* next-node (n/spaces target-spaces))

          :else
          ;; Insert new whitespace
          (z/insert-right* zloc (n/spaces target-spaces)))))))

;; -----------------------------------------------------------------------------
;; Comma rules

(defmethod apply-rules :comma [zloc]
  ;; Commas should be removed from sequential collections
  ;; But this needs context about parent node
  ;; For now, just return as-is
  zloc)

;; -----------------------------------------------------------------------------
;; List rules (function calls, special forms)

(defmethod apply-rules :list [zloc]
  ;; Will handle:
  ;; - Function argument alignment
  ;; - Special form formatting (let, cond, etc.)
  zloc)

;; -----------------------------------------------------------------------------
;; Vector rules

(defmethod apply-rules :vector [zloc]
  ;; Will handle:
  ;; - Destructuring alignment
  ;; - Vector literal formatting
  zloc)

;; -----------------------------------------------------------------------------
;; Map rules

(defmethod apply-rules :map [zloc]
  ;; Will handle:
  ;; - Key-value alignment
  ;; - Map literal formatting
  zloc)
