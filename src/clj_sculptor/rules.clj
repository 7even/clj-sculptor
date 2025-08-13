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

(defn function-call?
  "Check if a list represents a function call (first element is a symbol).
   Excludes special forms that have their own formatting rules."
  [zloc]
  (and (= (z/tag zloc)
          :list)
       (some-> zloc z/down z/sexpr symbol?)
       ;; Exclude special forms that should not use function argument alignment
       (not (contains? #{'let 'if 'when 'cond 'case 'for 'doseq 'dotimes
                         'defn 'defn- 'def 'defmacro 'defmethod 'defmulti
                         'ns 'require 'import 'use}
                       (some-> zloc z/down z/sexpr)))))

(defn get-function-first-arg-position
  "Get the column position of the first argument in a function call.
   Returns nil if not a function call or first arg is on next line."
  [list-zloc]
  (when (function-call? list-zloc)
    (let [func-name (some-> list-zloc z/down)
          ;; Use z/right to skip whitespace/newlines and get first actual argument
          first-arg (some-> func-name z/right)]
      (when (and func-name first-arg)
        (let [[func-line func-col] (z/position func-name)
              [arg-line arg-col] (z/position first-arg)]
          ;; Only return position if first arg is on same line as function name
          (when (= func-line arg-line)
            arg-col))))))

(defn function-arg-alignment-spaces
  "Calculate alignment spaces for function arguments.
   Since first arg is always moved to same line as function name,
   we always align with the first argument position."
  [zloc]
  (when-let [parent-list (some-> zloc z/up)]
    (when (function-call? parent-list)
      (when-let [first-arg-col (get-function-first-arg-position parent-list)]
        ;; Align with the first argument position
        (max 0 (dec first-arg-col))))))

(defn newline-after-function-name?
  "Check if this newline is immediately after a function name in a function call."
  [newline-zloc]
  (when-let [prev-token (z/prev* newline-zloc)]
    (when (= (z/tag prev-token)
             :token)
      (when-let [parent (z/up newline-zloc)]
        (and (function-call? parent)
             ;; Check if prev-token is the function name (first child of parent)
             (= prev-token
                (z/down parent)))))))

(defn whitespace-after-function-newline?
  "Check if this whitespace follows a newline that's after a function name."
  [whitespace-zloc]
  (when-let [prev-node (z/prev* whitespace-zloc)]
    (when (= (z/tag prev-node)
             :newline)
      (newline-after-function-name? prev-node))))

(defn whitespace-between-function-args?
  "Check if this whitespace is between arguments in a function call on the same line."
  [whitespace-zloc]
  (when-let [prev-node (z/left* whitespace-zloc)]
    (when-let [next-node (z/right* whitespace-zloc)]
      ;; Check if both neighbors are non-whitespace/non-newline nodes (any kind of argument)
      (when (and (not (#{:whitespace :newline :comma} (z/tag prev-node)))
                 (not (#{:whitespace :newline :comma} (z/tag next-node))))
        ;; Check if we're in a function call
        (when-let [parent (z/up whitespace-zloc)]
          (when (function-call? parent)
            ;; Check if prev-node is NOT the function name (first child)
            (let [func-name (z/down parent)]
              (not= prev-node func-name))))))))

(defn calculate-alignment-spaces
  "Calculate spaces needed for position-based alignment.
   Returns the number of spaces to indent this element."
  [zloc]
  (or (function-arg-alignment-spaces zloc)
      (when-let [target-col (and (some-> zloc
                                         find-collection-parent
                                         z/tag
                                         #{:map :vector :set})
                                 (get-alignment-column zloc))]
        (max 0 (dec target-col)))
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

      ;; Whitespace between function args on same line - replace with newline + indentation
      (whitespace-between-function-args? zloc)
      (let [next-arg (z/right* zloc)
            alignment-spaces (calculate-alignment-spaces next-arg)]
        ;; Replace whitespace with newline followed by proper indentation
        (-> zloc
            (z/replace* (n/newlines 1))
            (z/insert-right* (n/spaces alignment-spaces))))

      ;; Whitespace after function name (that was converted from newline) - remove it
      (whitespace-after-function-newline? zloc)
      (z/remove* zloc)

      ;; Indentation whitespace - handled by :newline rule (only for actual newlines)
      (let [prev (z/prev* zloc)]
        (and (= (z/tag prev)
                :newline)
             (not (newline-after-function-name? prev))))
      zloc

      ;; Trailing whitespace - remove entirely
      (or (z/end? (z/next* zloc))
          (= (z/tag (z/next* zloc))
             :newline))
      (z/remove* zloc)

      ;; Multiple spaces between tokens - normalize to single space
      (> (count content)
         1)
      (z/replace* zloc
                  (n/spaces 1))

      :else
      zloc)))

(defmethod apply-rules :newline [zloc]
  (cond
    ;; Newline after function name - replace with space and handle following whitespace
    (newline-after-function-name? zloc)
    (let [next-node (z/next* zloc)]
      (if (and (some? next-node)
               (= (z/tag next-node)
                  :whitespace))
        ;; Replace newline with space and remove the following whitespace
        (-> zloc
            (z/replace* (n/spaces 1))
            z/next*
            z/remove*)
        ;; Just replace newline with space
        (z/replace* zloc (n/spaces 1))))

    ;; Skip empty lines and end of file
    (let [next-node (z/next* zloc)]
      (or (nil? next-node)
          (= (z/tag next-node)
             :newline)))
    zloc

    ;; Handle indentation for any content after newline
    :else
    (let [next-node (z/next* zloc)
          target-node (if (= (z/tag next-node) :whitespace)
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
        (z/replace* next-node
                    (n/spaces target-spaces))

        :else
        ;; Insert new whitespace
        (z/insert-right* zloc
                         (n/spaces target-spaces))))))

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
  ;; Function call formatting is handled by whitespace and newline rules
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
