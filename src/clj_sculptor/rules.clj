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
    'require 'import 'use})

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

(def ns-form?
  "ns form."
  #{'ns})

(defn ns-arg?
  "Check if we're an argument to an ns form."
  [zloc]
  (let [parent (z/up zloc)]
    (and (some? parent)
         (= (z/tag parent)
            :list)
         (some-> parent z/down z/sexpr ns-form?))))

(def require-form?
  "require and import forms in ns declaration."
  #{:require :import})

;; -----------------------------------------------------------------------------
;; Phase 1: Strip all whitespace and newlines

(defn- to-leftmost-at-root
  "Navigate to the leftmost position at the top level of the tree."
  [zloc]
  (-> zloc
      z/root
      (z/of-node* {:track-position? true})))

(defn normalize-collection-types
  "Normalize collection types in require/import forms:
   - :require should use vectors [...]
   - :import should use lists (...)"
  [zloc]
  (let [start-loc (to-leftmost-at-root zloc)]
    (loop [loc start-loc]
      (let [parent (z/up loc)
            parent-tag (when (some? parent)
                         (z/tag parent))
            require-type (some-> parent z/up z/down z/sexpr)]
        (cond
          (z/end? loc)
          ;; Create a new zipper from the modified tree
          (z/of-node (z/root loc) {:track-position? true})

          ;; Lists in :require should become vectors
          (and (in-require-form? loc)
               (= require-type :require)
               (= parent-tag :list)
               ;; Don't convert the :require list itself, only sublists
               (not= (some-> parent z/down z/sexpr) :require))
          (let [required-namespaces (z/child-sexprs parent)
                vector-node (n/vector-node (map n/token-node required-namespaces))
                new-loc (z/replace parent vector-node)]
            (recur new-loc))

          ;; Vectors in :import should become lists
          (and (in-require-form? loc)
               (= require-type :import)
               (= parent-tag :vector))
          (let [imported-namespaces (z/child-sexprs parent)
                list-node (n/list-node (map n/token-node imported-namespaces))
                new-loc (z/replace parent list-node)]
            (recur new-loc))

          :else
          (recur (z/next* loc)))))))

(defn strip-whitespace
  "Remove all whitespace, newline, and comma nodes from the zipper.
   Returns a zipper positioned at the beginning of the cleaned tree."
  [zloc]
  (let [start-loc (to-leftmost-at-root zloc)]
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
  "Check if a list represents a function call or function-like form (not a special form)."
  [zloc]
  (and (= (z/tag zloc)
          :list)
       (let [first-elem (some-> zloc z/down z/sexpr)]
         (or (and (symbol? first-elem)
                  (not (special-form? first-elem)))
             (keyword? first-elem)))))

(defn in-require-form?
  "Check if we're inside a :require or :import form."
  [zloc]
  (loop [loc zloc]
    (if-let [parent (z/up loc)]
      (if (and (= (z/tag parent)
                  :list)
               (some-> parent z/down z/sexpr require-form?))
        true
        (recur parent))
      false)))

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
   - :if-clause
   - :require-vector-element
   - :ns-arg"
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

      ;; Special handling for :require/:import vectors - check early!
      (and (in-require-form? zloc)
           (= parent-tag :vector)
           (not leftmost?))
      :require-vector-element

      ;; Special handling for import lists (java.util Date Calendar)
      ;; These are lists inside :import forms, treat like vectors
      (and (in-require-form? zloc)
           (= parent-tag :list)
           (not leftmost?)
           ;; Check if parent list's first element is a package symbol (not :import)
           (let [first-elem (some-> parent z/down z/sexpr)]
             (and (symbol? first-elem)
                  (not (require-form? first-elem)))))
      :require-vector-element

      ;; NS form arguments (except first) get special 2-space indentation
      (and (ns-arg? zloc)
           (not leftmost?)
           ;; Not the first argument (namespace name)
           (not (= (z/left zloc)
                   (-> zloc z/up z/down))))
      :ns-arg

      ;; Function arguments (check before other list handling)
      (and (function-call? parent)
           (= (z/left zloc)
              (z/down parent)))
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

      :require-vector-element
      ;; Elements inside require/import vectors get single space
      (n/spaces 1)

      :ns-arg
      ;; NS form arguments get newline and 2-space indentation
      [(n/newlines 1) (n/spaces 2)]

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

;; -----------------------------------------------------------------------------
;; Namespace ordering functions

(defn sort-require-form
  "Sort namespaces alphabetically within a :require form."
  [form-node]
  (if (and (= (n/tag form-node)
              :list)
           (= (some-> form-node n/children first n/sexpr)
              :require))
    (let [children (n/children form-node)
          [keyword & ns-specs] children
          non-whitespace-specs (filter #(not (whitespace? (n/tag %)))
                                       ns-specs)
          sorted-ns-specs (sort-by #(if (= (n/tag %)
                                           :list)
                                      (str (some-> % n/children first n/sexpr))
                                      (str (n/sexpr %)))
                                   non-whitespace-specs)]
      (n/list-node (cons keyword sorted-ns-specs)))
    form-node))

(defn sort-import-form
  "Sort import specs alphabetically within an :import form."
  [form-node]
  (if (and (= (n/tag form-node)
              :list)
           (= (some-> form-node n/children first n/sexpr)
              :import))
    (let [children (n/children form-node)
          [keyword & import-specs] children
          non-whitespace-specs (filter #(not (whitespace? (n/tag %)))
                                       import-specs)
          sorted-import-specs (sort-by #(if (= (n/tag %)
                                               :list)
                                          (str (some-> % n/children first n/sexpr))
                                          (str (n/sexpr %)))
                                       non-whitespace-specs)]
      (n/list-node (cons keyword sorted-import-specs)))
    form-node))

(defn sort-ns-forms
  "Sort :require and :import forms in ns declaration, putting :require first."
  [zloc]
  (if (and (= (z/tag zloc)
              :list)
           (= (some-> zloc z/down z/sexpr)
              'ns))
    (let [forms (-> zloc z/node n/children)
          non-ns-forms (filter #(not (and (= (n/tag %)
                                             :list)
                                          (let [first-sym (some-> % n/children first n/sexpr)]
                                            (or (= first-sym
                                                   :require)
                                                (= first-sym
                                                   :import)))))
                               forms)
          ns-forms (filter #(and (= (n/tag %)
                                    :list)
                                 (let [first-sym (some-> % n/children first n/sexpr)]
                                   (or (= first-sym
                                          :require)
                                       (= first-sym
                                          :import))))
                           forms)
          requires (filter #(= (some-> % n/children first n/sexpr)
                               :require)
                           ns-forms)
          imports (filter #(= (some-> % n/children first n/sexpr)
                              :import)
                          ns-forms)
          sorted-requires (map sort-require-form requires)
          sorted-imports (map sort-import-form imports)
          ordered-forms (concat non-ns-forms sorted-requires sorted-imports)]
      (z/replace zloc (n/list-node ordered-forms)))
    zloc))
