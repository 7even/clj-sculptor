(ns clj-sculptor.rules
  "Formatting rules using strip-and-generate approach."
  (:require [clojure.string :as str]
            [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]))

;; -----------------------------------------------------------------------------
;; Constants and predicates

(def special-form?
  "Special forms that have their own formatting rules."
  #{'let 'if 'when 'cond 'case 'for 'doseq 'dotimes
    'defn 'defn- 'def 'defmacro 'defmethod 'defmulti
    'ns 'require 'import 'use})

;; Predicate sets for node type classification
(def collection-node?
  "Collection node types that can contain elements."
  #{:vector :map :set})

(def container-node?
  "All container node types including lists."
  #{:list :vector :map :set})

(def whitespace?
  "Whitespace and newline node types."
  #{:whitespace :newline})

(def strippable?
  "Node types that should be stripped during cleaning."
  #{:whitespace :newline :comma})

(def defn-form?
  "Function definition forms."
  #{'defn 'defn-})

(def defmethod-form?
  "defmethod form."
  #{'defmethod})

(def let-form?
  "let form."
  #{'let})

(def if-form?
  "if form."
  #{'if})

;; -----------------------------------------------------------------------------
;; Phase 1: Strip all whitespace and newlines

(defn strip-whitespace
  "Remove all whitespace, newline, and comma nodes from the zipper.
   Returns a zipper positioned at the beginning of the cleaned tree."
  [zloc]
  ;; First, navigate to the absolute beginning (leftmost at top level)
  (let [start-loc (loop [loc zloc]
                    (if-let [up (z/up loc)]
                      (recur up)
                      ;; At top, go to leftmost
                      (loop [l loc]
                        (if-let [left (z/left* l)]
                          (recur left)
                          l))))]
    (loop [loc start-loc]
      (cond
        (z/end? loc)
        ;; Create a new zipper from the modified tree with position tracking
        (z/of-node (z/root loc) {:track-position? true})

        (strippable? (z/tag loc))
        (recur (z/remove* loc))

        :else
        (recur (z/next* loc))))))

;; -----------------------------------------------------------------------------
;; Phase 2: Context detection helpers

(defn function-call?
  "Check if a list represents a function call (not a special form)."
  [zloc]
  (and (= (z/tag zloc) :list)
       (some-> zloc z/down z/sexpr symbol?)
       (not (special-form? (some-> zloc z/down z/sexpr)))))

(defn get-position-context
  "Determine what kind of position we're at for whitespace generation.
   Returns one of:
   - :after-open-paren
   - :between-elements
   - :first-function-arg
   - :subsequent-function-arg
   - :collection-element
   - :map-value
   - :after-defn-args
   - :after-defmethod-args
   - :after-let-bindings
   - :if-clause"
  [zloc]
  (let [tag (z/tag zloc)
        parent (z/up zloc)
        parent-tag (when parent (z/tag parent))
        leftmost? (nil? (z/left zloc))
        rightmost? (nil? (z/right zloc))]
    (cond
      ;; After opening delimiter
      (and leftmost?
           (container-node? parent-tag))
      :after-open-paren

      ;; Function arguments (check before other list handling)
      (and (function-call? parent)
           (= (z/left zloc) (z/down parent)))
      :first-function-arg

      (function-call? parent)
      :subsequent-function-arg

      ;; After defn/defn- args list - check if this is a body element after args
      (and parent
           (= parent-tag
              :list)
           (let [parent-first (some-> parent z/down z/sexpr)]
             (and (defn-form? parent-first)
                  ;; Check if previous sibling is a vector (args list)
                  (= (some-> zloc z/left z/tag)
                     :vector))))
      :after-defn-args

      ;; After defmethod args list - check if this is a body element after args
      (and parent
           (= parent-tag
              :list)
           (let [parent-first (some-> parent z/down z/sexpr)]
             (and (defmethod-form? parent-first)
                  ;; Check if previous sibling is a vector (args list)
                  (= (some-> zloc z/left z/tag)
                     :vector))))
      :after-defmethod-args

      ;; After let bindings vector
      (and parent
           (= parent-tag
              :list)
           (let [parent-first (some-> parent z/down z/sexpr)]
             (and (let-form? parent-first)
                  ;; Check if previous sibling is a vector (bindings)
                  (= (some-> zloc z/left z/tag)
                     :vector))))
      :after-let-bindings

      ;; If statement clauses - check if this is a then/else clause after condition
      (and parent
           (= parent-tag
              :list)
           (let [parent-first (some-> parent z/down z/sexpr)]
             (and (if-form? parent-first)
                  ;; Check if we're not the condition (first argument)
                  (not= (z/left zloc) (z/down parent)))))
      :if-clause

      ;; Map values (every even position in map after first, counting only non-whitespace)
      (and (= parent-tag
              :map)
           (not leftmost?)
           (let [pos (loop [curr (z/leftmost zloc)
                            count 0]
                       (if (= curr zloc)
                         count
                         (recur (z/right curr) (inc count))))] ;; Use z/right to skip whitespace
             (odd? pos))) ;; Values are at odd positions (1, 3, 5...)
      :map-value

      ;; Special case: function calls in let binding vectors get single space
      (and (= tag
              :list)
           (= parent-tag
              :vector)
           (let [grandparent (z/up parent)]
             (and grandparent
                  (= (z/tag grandparent)
                     :list)
                  (let-form? (some-> grandparent z/down z/sexpr)))))
      :map-value

      ;; Collection elements (any non-first element in collections)
      (and (collection-node? parent-tag)
           (not leftmost?))
      :collection-element

      ;; Between elements
      :else
      :between-elements)))

(defn get-first-arg-column
  "Get the column position where the first function argument starts."
  [list-zloc]
  (when (function-call? list-zloc)
    (let [func-name (z/down list-zloc)
          func-str (str (z/sexpr func-name))
          ;; Add 2 for opening paren and space after function name
          base-col (-> list-zloc z/position second)]
      (+ base-col 1 (count func-str) 1))))

;; -----------------------------------------------------------------------------
;; Phase 3: Whitespace generation

(defn generate-whitespace-before
  "Generate appropriate whitespace to insert before a node."
  [zloc]
  (let [context (get-position-context zloc)
        parent (z/up zloc)]
    (case context
      :after-open-paren
      nil ;; No whitespace after opening paren

      :first-function-arg
      (n/spaces 1) ;; Single space after function name

      :subsequent-function-arg
      ;; Newline and indent to align with first arg
      (let [first-arg-col (get-first-arg-column parent)]
        [(n/newlines 1) (n/spaces (dec first-arg-col))])

      :after-defn-args
      ;; Newline and indent after defn args list
      ;; Use 3 spaces if defn is nested in a collection, 2 spaces otherwise
      (let [defn-parent (z/up parent)
            indent (if (and defn-parent
                            (collection-node? (z/tag defn-parent)))
                     3 ;; 3 spaces when nested in collection
                     2)] ;; 2 spaces normally
        [(n/newlines 1) (n/spaces indent)])

      :after-defmethod-args
      ;; Newline and indent after defmethod args list
      ;; Use 3 spaces if defmethod is nested in a collection, 2 spaces otherwise
      (let [defmethod-parent (z/up parent)
            indent (if (and defmethod-parent
                            (collection-node? (z/tag defmethod-parent)))
                     3 ;; 3 spaces when nested in collection
                     2)] ;; 2 spaces normally
        [(n/newlines 1) (n/spaces indent)])

      :after-let-bindings
      ;; Newline and 2-space indent after let bindings vector
      [(n/newlines 1) (n/spaces 2)]

      :if-clause
      ;; If then/else clauses get newline and 4-space indent (2 + 2 for if body)
      [(n/newlines 1) (n/spaces 4)]

      :map-value
      ;; Map values get a single space after the key
      (n/spaces 1)

      :collection-element
      ;; Collection elements align with first element position
      (let [parent-tag (z/tag parent)
            first-elem (z/down parent)
            first-elem-col (when (some? first-elem)
                             (-> first-elem z/position second))]
        (if first-elem-col
          ;; Convert column to spaces (column - 1)
          [(n/newlines 1) (n/spaces (dec first-elem-col))]
          ;; Fallback - use 2 spaces for sets, 1 space for others
          [(n/newlines 1) (n/spaces (if (= parent-tag :set) 2 1))]))

      :between-elements
      ;; Top-level forms need double newlines between them (1 empty line)
      (if (= (z/tag parent)
             :forms)
        [(n/newlines 1) (n/newlines 1)]
        (n/spaces 1)))))

(defn insert-whitespace
  "Walk the tree and insert appropriate whitespace."
  [zloc]
  (loop [loc zloc]
    (cond
      (z/end? loc)
      loc

      ;; Skip whitespace-like nodes (shouldn't be any after stripping)
      ;; Skip newly inserted whitespace nodes
      (whitespace? (z/tag loc))
      (recur (z/next* loc))

      ;; For each node, check if we need whitespace before it
      :else
      (let [left-node (z/left* loc) ;; Use left* consistently
            needs-whitespace? (and left-node
                                   (not (whitespace? (z/tag left-node))))
            whitespace (when needs-whitespace?
                         (generate-whitespace-before loc))]
        (if whitespace
          (if (sequential? whitespace)
            ;; Handle sequence of whitespace nodes
            (let [loc-with-ws (reduce (fn [l ws-node]
                                        (z/insert-left* l ws-node))
                                      loc
                                      whitespace)]
              (recur (z/next* loc-with-ws)))
            ;; Handle single whitespace node
            (recur (z/next* (z/insert-left* loc whitespace))))
          (recur (z/next* loc)))))))
