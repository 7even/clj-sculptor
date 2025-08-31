(ns clj-sculptor.rules
  "Formatting rules using recursive multimethod architecture for tree processing."
  (:require [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [clojure.string :as str]))

;; Node type predicates
(def strippable?
  "Node types that should be stripped during cleaning."
  #{:whitespace :newline :comma})

(defn- comment? [node]
  (and (some? node)
       (= (n/tag node)
          :comment)))

(defn- standalone-comment?
  "Check if an item-map represents a standalone comment."
  [item-map]
  (-> item-map
      :element
      comment?))

(defn- is-token?
  "Check if a node is a token."
  [node]
  (= (n/tag node) :token))

(defn- token-matches?
  "Check if a node is a token with the given string value."
  [node value]
  (and (is-token? node)
       (= (n/string node) value)))

;; Helper functions
(defn- extract-leading-comments
  "Extracts any standalone comments that appear before the first non-comment element.
   Returns [comment-maps remaining-elements]."
  [item-maps]
  (let [comments (take-while standalone-comment? item-maps)
        remaining (drop-while standalone-comment? item-maps)]
    [(vec comments)
     (vec remaining)]))

(defn- group-with-following
  "Groups elements with the following element based on a predicate.
   When predicate returns true for an element, it gets grouped with the next element.
   Returns a sequence where each item is either:
   - A single element (for standalone elements)
   - A vector [trigger-element following-element] for grouped pairs

   Example: (group-with-following #(= % '&) '[a & rest b])
           => '(a [& rest] b)"
  [pred elements]
  (->> elements
       (reduce (fn [{:keys [result pending]} elem]
                 (cond
                   ;; Current element matches predicate - hold as pending
                   (pred elem)
                   {:result result
                    :pending elem}

                   ;; We have a pending element - group it with current
                   (some? pending)
                   {:result (conj result [pending elem])
                    :pending nil}

                   ;; Normal element - add as single
                   :else
                   {:result (conj result elem)
                    :pending nil}))
               {:result []
                :pending nil})
       :result))

(defn- group-with-prefixes-and-comments
  "Groups elements with optional prefixes (like &) and inline comments in one pass.
   Processes raw children directly, handling whitespace and grouping logic.
   Returns a sequence where each item is a map with:
   - :element - the main element
   - :prefix - optional prefix element (like &)
   - :comment - optional inline comment

   Examples:
   x                => {:element x}
   x ;; comment     => {:element x
                        :comment comment}
   & rest           => {:element rest
                        :prefix &}
   & rest ;; comment => {:element rest
                         :prefix &
                         :comment comment}
   ;; standalone     => {:element comment} (standalone comments)"
  [children]
  (let [final-state (reduce (fn [{:keys [result
                                         current-element
                                         pending-prefix
                                         had-newline?]
                                  :as state}
                                 child]
                              (case (n/tag child)
                                :newline
                                (cond-> state
                                  (some? current-element) (assoc :had-newline? true))

                                (:whitespace :comma)
                                state

                                :comment
                                (if (and (some? current-element)
                                         (not had-newline?))
                                  ;; Inline comment - attach to current element
                                  (let [element-map (assoc current-element :comment child)]
                                    (-> state
                                        (update :result conj element-map)
                                        (assoc :current-element nil)))
                                  ;; Standalone comment - add current element, then comment
                                  (-> state
                                      (update :result (fn [result]
                                                        (cond-> result
                                                          (some? current-element)
                                                          (conj current-element)

                                                          :always
                                                          (conj {:element child}))))
                                      (assoc :current-element nil)))

                                ;; Regular element
                                (cond
                                  ;; Current element is & - save as pending prefix
                                  (and (is-token? child)
                                       (= (n/string child) "&"))
                                  (-> state
                                      (update :result (fn [result]
                                                        (cond-> result
                                                          (some? current-element)
                                                          (conj current-element))))
                                      (assoc :current-element nil
                                             :pending-prefix child
                                             :had-newline? false))

                                  ;; We have a pending & prefix - create prefixed element
                                  (some? pending-prefix)
                                  (assoc state
                                         :current-element {:element child
                                                           :prefix pending-prefix}
                                         :pending-prefix nil
                                         :had-newline? false)

                                  ;; Regular element - add previous element, set this as current
                                  :else
                                  (-> state
                                      (update :result (fn [result]
                                                        (cond-> result
                                                          (some? current-element)
                                                          (conj current-element))))
                                      (assoc :current-element {:element child}
                                             :had-newline? false)))))
                            {:result []
                             :current-element nil
                             :pending-prefix nil
                             :had-newline? false}
                            children)]
    ;; Add any remaining current element
    (cond-> (:result final-state)
      (some? (:current-element final-state))
      (conj (:current-element final-state)))))

(defn- make-separator
  "Create a separator with newline and indented spaces."
  [indent-level]
  [(n/newlines 1) (n/spaces indent-level)])

(defn- add-closing-bracket-spacing
  "Adds appropriate spacing before closing bracket if last element has a comment.
   Takes:
   - elements: sequence of formatted elements
   - indent: the indent level for the closing bracket

   Returns the elements with closing bracket spacing added if needed."
  [elements indent]
  (let [last-element (last elements)
        ;; Check if last element is a comment or ends with a comment
        last-has-comment? (or (comment? last-element)
                              (when (n/inner? last-element)
                                (-> last-element
                                    n/children
                                    last
                                    comment?)))]
    (if last-has-comment?
      (if (> indent 0)
        (concat elements [(n/newlines 1) (n/spaces indent)])
        (concat elements [(n/newlines 1)]))
      elements)))

(defn- analyze-multiline
  "Analyze a string for multi-line content and return info map."
  [s]
  (let [lines (str/split s #"\n")]
    {:multi-line? (> (count lines) 1)
     :last-line (last lines)}))

(defmulti format-node
  "Recursively formats a node."
  (fn [indent node]
    (n/tag node)))

(defn- format-element-with-prefix-and-comment
  "Formats an element map that may have :prefix and/or :comment.
   Map structure: {:element elem :prefix prefix :comment comment}
   Returns a formatted node or n/forms-node with proper spacing."
  [base-indent item-map]
  (let [{:keys [element prefix comment]} item-map]
    ;; If we have prefix or comment, build a forms-node with all parts
    (if (or (some? prefix)
            (some? comment))
      (cond-> []
        ;; Add prefix if present (like &)
        (some? prefix)
        (conj (format-node base-indent prefix)
              (n/spaces 1))

        ;; Always add the main element
        ;; If there's a prefix, the element is visually positioned after the prefix + space
        :always
        (conj (format-node (if (some? prefix)
                             (+ base-indent 2) ;; Account for "& "
                             base-indent)
                           element))

        ;; Add comment if present
        (some? comment)
        (conj (n/spaces 1)
              (format-node base-indent comment))

        :always
        n/forms-node)

      ;; Otherwise just return the formatted element
      (format-node base-indent element))))

(defn- build-pairs
  "Build structured pairs from grouped elements, handling comments according to rules.
   Works for both maps and binding vectors.

   Input:  [{:element :a}
            {:element 1
             :comment ; comment}
            {:element ;; block}
            {:element :b}
            {:element 2}]
   Output: [{:type :pair
             :key-item {:element :a}
             :value-item {:element 1
                          :comment ; comment}}
            {:type :comment
             :node ;; block}
            {:type :pair
             :key-item {:element :b}
             :value-item {:element 2}}]"
  [grouped]
  (loop [items grouped
         result []]
    (if (empty? items)
      result
      (let [[current & remaining] items
            last-entry (peek result)]
        (cond
          ;; Standalone comment
          (standalone-comment? current)
          (let [next-item (first remaining)]
            (cond
              ;; Comment between key and value - move before the pair
              (and (some? last-entry)
                   (= (:type last-entry)
                      :key-waiting)
                   (not (empty? remaining))
                   (not (standalone-comment? next-item)))
              (let [value-item next-item
                    new-remaining (rest remaining)
                    key-item (:key-item last-entry)]
                (recur new-remaining
                       (-> (pop result) ; Remove the key-waiting entry
                           (conj {:type :comment
                                  :node (:element current)})
                           (conj {:type :pair
                                  :key-item key-item
                                  :value-item value-item}))))

              ;; Normal standalone comment - keep in place
              :else
              (recur remaining
                     (conj result {:type :comment
                                   :node (:element current)}))))

          ;; Element with inline comment
          (:comment current)
          (let [next-item (first remaining)]
            (cond
              ;; If there's a waiting key, this element with comment is a value
              (and (some? last-entry)
                   (= (:type last-entry)
                      :key-waiting))
              (recur remaining
                     (conj (pop result)
                           {:type :pair
                            :key-item (:key-item last-entry)
                            :value-item current}))

              ;; If followed by a non-comment element, it's a key with inline comment
              (and (not (empty? remaining))
                   (not (standalone-comment? next-item))
                   (nil? (:comment next-item)))
              (let [value-item next-item
                    new-remaining (rest remaining)]
                (recur new-remaining
                       (-> result
                           (conj {:type :comment
                                  :node (:comment current)})
                           (conj {:type :pair
                                  :key-item {:element (:element current)}
                                  :value-item value-item}))))

              ;; Otherwise it's an orphaned key with inline comment
              :else
              (recur remaining
                     (conj result {:type :key-waiting
                                   :key-item current}))))

          ;; Regular element without inline comment - key-value pairing logic
          :else
          (recur remaining
                 (let [last-entry (peek result)]
                   (cond
                     ;; Previous was a key waiting for value - make a pair
                     (and (some? last-entry)
                          (= (:type last-entry)
                             :key-waiting))
                     (conj (pop result)
                           {:type :pair
                            :key-item (:key-item last-entry)
                            :value-item current})

                     ;; No waiting key - this element becomes a waiting key
                     :else
                     (conj result
                           {:type :key-waiting
                            :key-item current})))))))))

(defn- needs-trailing-spacing?
  "Check if we need trailing spacing after the last entry."
  [pairs]
  (and (not (empty? pairs))
       (let [last-pair (peek pairs)]
         (or (= (:type last-pair)
                :comment)
             (and (= (:type last-pair)
                     :pair)
                  (-> last-pair
                      :value-item
                      :comment
                      some?))))))

(defn- format-pair-comment
  "Format a standalone comment with optional trailing spacing.
   `item-indent` is the exact position where the comment should be aligned."
  [node item-indent is-last? needs-spacing?]
  (let [formatted-comment (format-node item-indent node)]
    (if (and is-last?
             needs-spacing?)
      [formatted-comment (n/newlines 1) (n/spaces item-indent)]
      [formatted-comment])))

(defn- calculate-value-indent
  "Calculate the indent level for a value based on its key's position and content.
   `key-indent` is the exact position where the key is aligned."
  [key-string key-indent]
  (let [{:keys [multi-line? last-line]} (analyze-multiline key-string)]
    (if multi-line?
      (+ (count last-line) 1)
      (+ key-indent (count key-string) 1))))

(defn- format-pair
  "Format a key-value pair with proper spacing and comment handling.
   `item-indent` is the exact position where the key should be aligned."
  [key-item value-item item-indent is-last? needs-spacing?]
  (let [format-child (partial format-node item-indent)
        ;; Format the key
        formatted-key (format-child (:element key-item))
        key-string (n/string formatted-key)
        value-indent (calculate-value-indent key-string item-indent)

        ;; Format the value (handling inline comments if needed)
        formatted-value (if (:comment value-item)
                          (let [formatted-element (format-node value-indent
                                                               (:element value-item))
                                formatted-comment (format-node value-indent
                                                               (:comment value-item))]
                            [formatted-element (n/spaces 1) formatted-comment])
                          (format-node value-indent (:element value-item)))

        ;; Add trailing spacing if needed for last entry with inline comment
        needs-trailing? (and is-last?
                             needs-spacing?
                             (:comment value-item))]
    (if needs-trailing?
      [formatted-key (n/spaces 1) formatted-value (n/newlines 1) (n/spaces item-indent)]
      [formatted-key (n/spaces 1) formatted-value])))

(defn- format-pair-entries
  "Format all entries (pairs, comments, orphaned keys).
   Works for both maps and binding vectors.
   `entries-indent` is the exact position where the entries should be aligned."
  [entries-indent pairs]
  (let [format-child (partial format-node entries-indent)
        needs-spacing? (needs-trailing-spacing? pairs)]
    (map-indexed (fn [entry-idx entry]
                   (let [is-last? (= entry-idx (dec (count pairs)))]
                     (case (:type entry)
                       :comment
                       (format-pair-comment (:node entry)
                                            entries-indent
                                            is-last?
                                            needs-spacing?)

                       :pair
                       (format-pair (:key-item entry)
                                    (:value-item entry)
                                    entries-indent
                                    is-last?
                                    needs-spacing?)

                       :key-waiting
                       ;; Orphaned key
                       (format-element-with-prefix-and-comment entries-indent
                                                               (:key-item entry)))))
                 pairs)))

(defn- format-args-vector [node indent]
  "Formats vectors in function argument contexts (defn args, fn args, etc.)
   Handles & rest specially to keep & and rest on same line
   Uses positional alignment where all arguments align with each other"
  (if (not= (n/tag node)
            :vector)
    ;; Not a vector - fallback to regular formatting
    (format-node indent node)
    ;; Vector - apply special argument formatting with alignment
    (let [children (n/children node)
          cleaned (group-with-prefixes-and-comments children)]
      (if (empty? cleaned)
        (n/vector-node [])
        (let [;; Calculate alignment position like function calls
              ;; The opening [ is at indent, first element starts at indent + 1
              ;; All subsequent elements should align at the same position as first element
              first-element-align (+ indent 1)

              ;; Format each group - the new function already handles & prefixes
              formatted (map (partial format-element-with-prefix-and-comment
                                      first-element-align)
                             cleaned)

              ;; Destructure into first and rest
              [formatted-first & formatted-rest] formatted

              separator (make-separator first-element-align)
              all-elements (if (empty? formatted-rest)
                             [formatted-first]
                             (concat [formatted-first]
                                     separator
                                     (interpose separator formatted-rest)))

              ;; Add closing bracket spacing if needed
              final-elements (add-closing-bracket-spacing all-elements
                                                          first-element-align)]
          (->> final-elements flatten n/vector-node))))))

(defn format-def [form-type args indent]
  "Formats def forms: (def symbol value) or (def symbol docstring value)"
  (let [[symbol-name-map & value-maps] args
        symbol-name (:element symbol-name-map)
        formatted-values (map (partial format-element-with-prefix-and-comment
                                       (+ indent 2))
                              value-maps)
        separator (make-separator (+ indent 2))
        values-with-spacing (interpose separator formatted-values)]
    (->> values-with-spacing
         (concat [form-type
                  (n/spaces 1)
                  symbol-name
                  separator])
         flatten
         n/list-node)))

(defn format-defmulti
  "Formats defmulti forms: (defmulti name dispatch-fn)"
  [form-type args indent]
  (let [[name-map & dispatch-maps] args
        name-element (:element name-map)
        formatted-dispatch (map (partial format-element-with-prefix-and-comment
                                         (+ indent 2))
                                dispatch-maps)
        separator (make-separator (+ indent 2))
        dispatch-with-spacing (interpose separator formatted-dispatch)]
    (->> dispatch-with-spacing
         (concat [form-type
                  (n/spaces 1)
                  name-element
                  separator])
         flatten
         n/list-node)))

(defn format-defmethod
  "Formats defmethod forms: (defmethod name dispatch-val [args] body)"
  [form-type args indent]
  ;; args are: multimethod-name, dispatch-value, args-vector, body...
  (let [[method-name-map dispatch-val-map args-vec-map & body-maps] args
        method-name (:element method-name-map)
        dispatch-val (:element dispatch-val-map)
        args-vec (:element args-vec-map)
        ;; Format method name and dispatch value on same line
        method-str (n/string method-name)
        dispatch-str (n/string (format-node (+ indent 1) dispatch-val))
        ;; Calculate position for args vector - after "defmethod name dispatch-val "
        args-indent (+ indent 1 9 1 (count method-str) 1 (count dispatch-str) 1)
        formatted-args (format-args-vector args-vec args-indent)
        ;; Format body elements
        formatted-body (map (partial format-element-with-prefix-and-comment
                                     (+ indent 2))
                            body-maps)
        body-separator (make-separator (+ indent 2))]
    (->> (concat [form-type
                  (n/spaces 1)
                  method-name
                  (n/spaces 1)
                  (format-node (+ indent 1) dispatch-val)
                  (n/spaces 1)
                  formatted-args]
                 (when (seq formatted-body)
                   body-separator)
                 (when (seq formatted-body)
                   (interpose body-separator formatted-body)))
         flatten
         n/list-node)))

(defn format-binding-form [form-type args indent]
  "Formats binding forms: let, for, doseq, etc. with proper pair alignment"
  (let [[bindings-vec-map & body-maps] args
        bindings-vec (:element bindings-vec-map)]
    (if (= (n/tag bindings-vec)
           :vector)
      ;; Proper let form - format bindings using pair logic
      (let [bindings-children (n/children bindings-vec)
            bindings-grouped (group-with-prefixes-and-comments bindings-children)
            bindings-pairs (build-pairs bindings-grouped)
            ;; Calculate proper alignment for binding pairs
            form-name-str (n/string form-type)
            bindings-indent (+ indent 1 (count form-name-str) 1 1) ; paren + form + space + [
            formatted-bindings (->> (format-pair-entries bindings-indent bindings-pairs)
                                    (interpose (make-separator bindings-indent))
                                    flatten
                                    n/vector-node)
            ;; Format body elements
            formatted-body (map (partial format-element-with-prefix-and-comment
                                         (+ indent 2))
                                body-maps)
            body-separator (make-separator (+ indent 2))]
        (->> formatted-body
             (interpose body-separator)
             (concat [form-type
                      (n/spaces 1)
                      formatted-bindings
                      body-separator])
             flatten
             n/list-node))
      ;; Invalid binding form - fall back to regular list formatting
      (->> (map (comp (partial format-node indent)
                      :element)
                args)
           (cons form-type)
           n/list-node))))

(defn- format-arity
  "Format a single arity of a function: ([args] body...)"
  [args-vec-item body-elements indent]
  (let [format-child (partial format-node (+ indent 1))
        ;; Format args, handling inline comments and prefixes
        args-line (let [formatted-args (format-args-vector (:element args-vec-item)
                                                           (+ indent 1))]
                    (if (-> args-vec-item :comment some?)
                      (let [formatted-comment (format-child (:comment args-vec-item))]
                        [formatted-args (n/spaces 1) formatted-comment])
                      [formatted-args]))
        ;; Format body elements, handling inline comments and prefixes
        formatted-bodies (map (partial format-element-with-prefix-and-comment
                                       (+ indent 1))
                              body-elements)
        body-separator (make-separator (+ indent 1))]
    (if (empty? formatted-bodies)
      (n/list-node args-line)
      (->> formatted-bodies
           (interpose body-separator)
           (concat args-line body-separator)
           flatten
           n/list-node))))

(defn format-function-form [form-type args indent]
  "Formats defn/defn-/fn forms with proper argument parsing"
  (let [form-symbol (n/sexpr form-type)
        is-fn-form? (= form-symbol 'fn)

        ;; For fn forms, first arg might be name (symbol) or args vector
        ;; For defn forms, first arg is always name
        [name-map remaining-maps] (if is-fn-form?
                                    (let [first-arg (first args)]
                                      (if (and (some? first-arg)
                                               (= (-> first-arg :element n/tag)
                                                  :token))
                                        ;; Named fn: (fn name [args] ...)
                                        [first-arg (rest args)]
                                        ;; Anonymous fn: (fn [args] ...)
                                        [nil args]))
                                    ;; defn always has name
                                    [(first args) (rest args)])

        name-node (when (some? name-map)
                    (:element name-map))

        ;; Check for docstring (only relevant for defn forms)
        [docstring-map remaining-maps] (if (and (not is-fn-form?)
                                                (seq remaining-maps)
                                                (-> remaining-maps
                                                    first
                                                    :element
                                                    n/sexpr
                                                    string?))
                                         [(first remaining-maps) (rest remaining-maps)]
                                         [nil remaining-maps])
        docstring (when (some? docstring-map)
                    (:element docstring-map))

        ;; Check if this is multi-arity: first remaining element is a list (arity form)
        ;; rather than a vector (single args)
        multi-arity? (and (seq remaining-maps)
                          (= (-> remaining-maps first :element n/tag)
                             :list))

        ;; Anonymous fn multi-arity needs special indentation
        is-anonymous-fn-multi? (and is-fn-form?
                                    (nil? name-node)
                                    multi-arity?)

        elements (if multi-arity?
                   ;; Multi-arity function - use different indent for anonymous fn
                   (let [arity-indent (if is-anonymous-fn-multi?
                                        (+ indent 4)
                                        (+ indent 2))
                         arities (map (fn [arity-map]
                                        (let [arity-node (:element arity-map)
                                              children (n/children arity-node)
                                              grouped (group-with-prefixes-and-comments children)
                                              [args-vec & body] grouped]
                                          (format-arity args-vec body arity-indent)))
                                      remaining-maps)
                         arity-separator (make-separator arity-indent)]
                     (if is-anonymous-fn-multi?
                       ;; Anonymous fn multi-arity: first arity on same line
                       (concat [form-type (n/spaces 1) (first arities)]
                               (->> (rest arities)
                                    (interpose arity-separator)
                                    (concat [arity-separator])))
                       ;; Named fn or defn multi-arity: all arities on separate lines
                       (concat [form-type (n/spaces 1) name-node]
                               (when (some? docstring)
                                 [(n/newlines 1)
                                  (n/spaces (+ indent 2))
                                  (format-node indent docstring)])
                               (->> arities
                                    (interpose arity-separator)
                                    (concat [arity-separator])))))

                   ;; Single arity function
                   (let [[args-vec-map & body-maps] remaining-maps
                         args-vec (:element args-vec-map)
                         form-type-str (n/string form-type)

                         ;; Calculate proper alignment for arguments vector
                         args-align (cond
                                      ;; defn with docstring: args are on separate line
                                      (some? docstring)
                                      (+ indent 2)

                                      ;; fn or defn without docstring:
                                      ;; args follow name/form on same line
                                      (some? name-node)
                                      (let [name-str (n/string name-node)]
                                        (+ indent 1 (count form-type-str) 1 (count name-str) 1))

                                      ;; Anonymous fn: args follow form type on same line
                                      :else
                                      (+ indent 1 (count form-type-str) 1))

                         formatted-args (format-args-vector args-vec args-align)]
                     (concat [form-type]
                             (when name-node
                               [(n/spaces 1) name-node])
                             (if (some? docstring)
                               [(n/newlines 1)
                                (n/spaces (+ indent 2))
                                (format-node indent docstring)
                                (n/newlines 1)
                                (n/spaces (+ indent 2))
                                formatted-args]
                               [(n/spaces 1)
                                formatted-args])
                             (->> body-maps
                                  (map (partial format-element-with-prefix-and-comment
                                                (+ indent 2)))
                                  (interpose [(n/newlines 1)
                                              (n/spaces (+ indent 2))])
                                  (into [(n/newlines 1)
                                         (n/spaces (+ indent 2))])))))]
    (->> elements flatten n/list-node)))

(defn format-conditional-form [form-type args indent]
  "Formats conditional forms: if, when, when-not, if-not with 2-space body indent"
  (let [[condition-map & body-maps] args
        form-type-str (n/string form-type)
        ;; Calculate proper alignment for condition (like function arguments)
        condition-align (+ indent 1 (count form-type-str) 1)
        ;; Format condition with proper alignment
        formatted-condition (format-element-with-prefix-and-comment condition-align
                                                                    condition-map)
        ;; Format body elements with 2-space indent
        formatted-body (map (partial format-element-with-prefix-and-comment
                                     (+ indent 2))
                            body-maps)
        body-separator (make-separator (+ indent 2))]
    (->> formatted-body
         (interpose body-separator)
         (concat [form-type
                  (n/spaces 1)
                  formatted-condition
                  body-separator])
         flatten
         n/list-node)))

(defn format-try-form
  "Format try/catch/finally forms with proper indentation."
  [form-type args indent]
  (let [body-indent (+ indent 2)
        body-separator (make-separator body-indent)

        ;; Helper function to format catch/finally clauses
        format-clause (fn [item-map]
                        (let [elem (:element item-map)]
                          (if (and (= (n/tag elem) :list)
                                   (let [first-child (first (n/children elem))]
                                     (and (some? first-child)
                                          (= (n/tag first-child)
                                             :token)
                                          (contains? #{'catch 'finally}
                                                     (n/sexpr first-child)))))
                            ;; Special handling for catch/finally
                            (let [children (n/children elem)
                                  [clause-type & clause-args] (->> children
                                                                   (remove (comp strippable?
                                                                                 n/tag))
                                                                   vec)
                                  clause-body-indent (+ body-indent 2)
                                  clause-body-separator (make-separator clause-body-indent)]
                              (if (= (n/sexpr clause-type)
                                     'catch)
                                ;; Format catch: catch Exception e on same line, body indented
                                (let [[exc-type exc-var & body] clause-args
                                      formatted-body (map (partial format-node
                                                                   clause-body-indent)
                                                          body)
                                      interposed-body (when (seq formatted-body)
                                                        (->> formatted-body
                                                             (interpose clause-body-separator)
                                                             flatten
                                                             (concat clause-body-separator)))]
                                  (-> [clause-type
                                       (n/spaces 1)
                                       exc-type
                                       (n/spaces 1)
                                       exc-var]
                                      (concat interposed-body)
                                      (add-closing-bracket-spacing clause-body-indent)
                                      vec
                                      n/list-node))
                                ;; Format finally: body indented
                                (let [formatted-body (map (partial format-node
                                                                   clause-body-indent)
                                                          clause-args)
                                      interposed-body (when (seq formatted-body)
                                                        (->> formatted-body
                                                             (interpose clause-body-separator)
                                                             flatten
                                                             (concat clause-body-separator)))]
                                  (-> [clause-type]
                                      (concat interposed-body)
                                      (add-closing-bracket-spacing clause-body-indent)
                                      vec
                                      n/list-node))))
                            ;; Regular try body expression
                            (format-element-with-prefix-and-comment body-indent
                                                                    item-map))))]
    (->> args
         (map format-clause)
         (interpose body-separator)
         (concat [form-type body-separator])
         flatten
         n/list-node)))

(defn- process-pair-args-with-comments
  "Process pair arguments with sophisticated comment handling.
   Returns a vector of formatted elements with proper blank line separators."
  [pair-args indent]
  (let [entries-indent (+ indent 2)
        format-entry (partial format-element-with-prefix-and-comment entries-indent)
        body-separator (make-separator entries-indent)
        blank-line-separator [(n/newlines 2) (n/spaces entries-indent)]
        args-vec (vec pair-args)
        processed (reduce (fn [{:keys [result pending-blank? skip-next?]} [idx current]]
                            (cond
                              ;; Skip this element if it was already processed as a value
                              skip-next?
                              {:result result
                               :pending-blank? pending-blank?
                               :skip-next? false}

                              ;; Process standalone comment
                              (standalone-comment? current)
                              (let [formatted-comment (format-entry current)
                                    ;; Comments need to be followed by
                                    ;; newline + indent for next element
                                    comment-with-sep [formatted-comment body-separator]]
                                {:result (if pending-blank?
                                           (conj result
                                                 blank-line-separator
                                                 comment-with-sep)
                                           (conj result
                                                 comment-with-sep))
                                 :pending-blank? false
                                 :skip-next? false})

                              ;; Process non-comment element
                              :else
                              (let [next-elem (get args-vec (inc idx))]
                                (cond
                                  ;; No next element - orphaned key
                                  (nil? next-elem)
                                  (let [formatted-key (format-entry current)]
                                    {:result (if pending-blank?
                                               (conj result
                                                     blank-line-separator
                                                     formatted-key)
                                               (conj result
                                                     formatted-key))
                                     :pending-blank? false
                                     :skip-next? false})

                                  ;; Next is comment - orphaned key
                                  (standalone-comment? next-elem)
                                  (let [formatted-key (format-entry current)]
                                    {:result (if pending-blank?
                                               (conj result
                                                     blank-line-separator
                                                     formatted-key)
                                               (conj result
                                                     formatted-key))
                                     :pending-blank? true
                                     :skip-next? false})

                                  ;; Next is not comment - form a pair
                                  :else
                                  (let [formatted-key (format-entry current)
                                        formatted-value (format-entry next-elem)
                                        pair-content [formatted-key
                                                      body-separator
                                                      formatted-value]]
                                    {:result (if pending-blank?
                                               (conj result
                                                     blank-line-separator
                                                     pair-content)
                                               (conj result
                                                     pair-content))
                                     :pending-blank? true
                                     :skip-next? true})))))
                          {:result []
                           :pending-blank? false
                           :skip-next? false}
                          (map-indexed vector args-vec))]
    (:result processed)))

(defn format-pair-based-form [form-type args indent]
  "Formats pair-based forms like cond, case, condp with proper pair alignment
   and blank line separation."
  (let [form-type-str (n/string form-type)
        entries-indent (+ indent 2)
        format-entry (partial format-element-with-prefix-and-comment
                              entries-indent)
        body-separator (make-separator entries-indent)
        blank-line-separator [(n/newlines 2) (n/spaces entries-indent)]]
    (case form-type-str
      ("cond->" "cond->>")
      (let [[expr-map & pair-args] args
            expr-align (+ indent 1 (count form-type-str) 1)
            formatted-expr (format-element-with-prefix-and-comment expr-align expr-map)
            processed (process-pair-args-with-comments pair-args indent)]
        (->> (concat [form-type
                      (n/spaces 1)
                      formatted-expr
                      body-separator]
                     processed)
             flatten
             n/list-node))

      "condp"
      (let [[pred-map expr-map & pair-args] args
            pred-align (+ indent 1 (count form-type-str) 1)
            formatted-pred (format-element-with-prefix-and-comment pred-align pred-map)
            pred-string (n/string formatted-pred)
            expr-indent (calculate-value-indent pred-string pred-align)
            formatted-expr (format-element-with-prefix-and-comment expr-indent expr-map)
            ;; Process pairs in groups of two
            pair-groups (let [pairs (partition-all 2 pair-args)]
                          (reduce (fn [result pair]
                                    (if (= (count pair)
                                           2)
                                      (let [[key-arg value-arg] pair
                                            formatted-key (format-entry key-arg)
                                            formatted-value (format-entry value-arg)]
                                        (conj result [formatted-key
                                                      body-separator
                                                      formatted-value]))
                                      ;; Odd case - single orphaned element
                                      (conj result [(format-entry (first pair))])))
                                  []
                                  pairs))
            ;; Join pairs with blank line separators
            pairs-content (interpose blank-line-separator pair-groups)]
        (->> (concat [form-type
                      (n/spaces 1)
                      formatted-pred
                      (n/spaces 1)
                      formatted-expr
                      body-separator]
                     pairs-content)
             flatten
             n/list-node))

      "case"
      (let [[expr-map & pair-args] args
            expr-align (+ indent 1 (count form-type-str) 1)
            formatted-expr (format-element-with-prefix-and-comment expr-align expr-map)
            ;; Handle pairs and possible default value
            [pairs default-val] (if (odd? (count pair-args))
                                  [(butlast pair-args) (last pair-args)]
                                  [pair-args nil])
            processed (process-pair-args-with-comments pairs indent)
            formatted-default (when (some? default-val)
                                (format-element-with-prefix-and-comment entries-indent
                                                                        default-val))
            content-with-default (if (some? formatted-default)
                                   (concat blank-line-separator [formatted-default])
                                   [])]
        (->> (concat [form-type
                      (n/spaces 1)
                      formatted-expr
                      body-separator]
                     processed
                     content-with-default)
             flatten
             n/list-node))

      ;; Default case for cond and other similar forms
      (let [processed (process-pair-args-with-comments args indent)]
        (->> (concat [form-type
                      body-separator]
                     processed)
             flatten
             n/list-node)))))

(defn format-body-only-form
  "Formats forms where all expressions go on separate lines (do, comment).
  These forms have no arguments on the same line as the form name.
  Special case: when there's only an inline comment and no body,
  the comment stays on the same line as the form name."
  [form-type args indent]
  ;; Check if args has only one element that is a comment (from inline comment after form name)
  (if (and (= (count args) 1)
           (comment? (:element (first args))))
    ;; Special case: only inline comment, keep it on same line
    (let [comment-node (:element (first args))
          formatted-comment (format-node indent comment-node)
          body-indent (+ indent 2)
          elements [form-type (n/spaces 1) formatted-comment]
          final-elements (add-closing-bracket-spacing elements body-indent)]
      (n/list-node (flatten final-elements)))
    ;; Regular case: format all body elements on separate lines
    (let [body-indent (+ indent 2)
          body-separator (make-separator body-indent)
          formatted-body (map (partial format-element-with-prefix-and-comment body-indent)
                              args)]
      (-> (concat [form-type body-separator]
                  (interpose body-separator formatted-body))
          (add-closing-bracket-spacing body-indent)
          flatten
          n/list-node))))

(defn- format-import-list
  "Format an import list (java.util Date Calendar) with proper class alignment"
  [import-node base-indent]
  (if (= (n/tag import-node)
         :list)
    (let [children (n/children import-node)
          cleaned (remove (comp strippable? n/tag)
                          children)
          [package & classes] cleaned
          package-str (n/string package)]
      (if (seq classes)
        (let [first-class (first classes)
              rest-classes (rest classes)
              ;; Calculate indent for aligned classes
              class-indent (+ base-indent 1 (count package-str) 1)]
          (n/list-node
           (concat [package
                    (n/spaces 1)
                    first-class]
                   (mapcat (fn [cls]
                             [(n/newlines 1)
                              (n/spaces class-indent)
                              cls])
                           rest-classes))))
        (n/list-node [package])))
    import-node))

(defn- format-require-clause
  "Format a :require clause with sorted entries, converting lists to vectors"
  [first-elem rest-elems indent]
  (let [;; Convert all collections to vectors and sort alphabetically
        sorted-entries (->> rest-elems
                            (filter (comp #{:vector :list}
                                          n/tag
                                          :element))
                            (map (fn [item]
                                   (let [elem (:element item)
                                         ;; Convert list to vector if needed
                                         as-vector (if (= (n/tag elem)
                                                          :list)
                                                     (-> elem n/children n/vector-node)
                                                     elem)]
                                     (assoc item :element as-vector))))
                            (sort-by (comp n/string :element)))
        formatted-first (format-element-with-prefix-and-comment (+ indent 3)
                                                                first-elem)]
    (if (seq sorted-entries)
      (let [first-entry (first sorted-entries)
            rest-entries (rest sorted-entries)
            ;; Don't format require vectors - keep them as-is
            formatted-first-entry (:element first-entry)
            ;; Calculate alignment for rest - align with first entry
            require-str (-> first-elem :element n/string)
            alignment-indent (+ indent 3 (count require-str) 1)
            ;; Keep rest entries as-is
            formatted-rest (map :element rest-entries)]
        (cond-> [formatted-first
                 (n/spaces 1)
                 formatted-first-entry]
          (seq formatted-rest)
          (concat (mapcat (fn [entry]
                            [(n/newlines 1)
                             (n/spaces alignment-indent)
                             entry])
                          formatted-rest))

          :always
          n/list-node))
      (n/list-node [formatted-first]))))

(defn- format-import-clause
  "Format an :import clause with sorted packages, converting vectors to lists"
  [first-elem rest-elems indent]
  (let [;; Convert all collections to lists and sort alphabetically
        import-lists (->> rest-elems
                          (filter (comp #{:vector :list}
                                        n/tag
                                        :element))
                          (map (fn [item]
                                 (let [elem (:element item)
                                       ;; Convert vector to list if needed
                                       as-list (if (= (n/tag elem)
                                                      :vector)
                                                 (n/list-node (n/children elem))
                                                 elem)]
                                   (assoc item :element as-list))))
                          (sort-by (comp n/string :element)))
        formatted-first (format-element-with-prefix-and-comment (+ indent 3)
                                                                first-elem)]
    (if (seq import-lists)
      (let [;; All import lists should align at the same column
            ;; which is after the ":import " keyword
            import-str (-> first-elem :element n/string)
            import-list-indent (+ indent 3 (count import-str) 1)
            ;; Format all import lists with the same base indent
            [first-list & rest-lists] import-lists
            formatted-first-list (format-import-list (:element first-list)
                                                     import-list-indent)
            formatted-rest (map (fn [import-item]
                                  (format-import-list (:element import-item)
                                                      import-list-indent))
                                rest-lists)]
        (cond-> [formatted-first
                 (n/spaces 1)
                 formatted-first-list]
          (seq formatted-rest)
          (concat (mapcat (fn [entry]
                            [(n/newlines 1)
                             (n/spaces import-list-indent)
                             entry])
                          formatted-rest))

          :always
          n/list-node))
      (n/list-node [formatted-first]))))

(defn- format-ns-clause
  "Format a single clause in an ns form (:require, :import, etc.)"
  [indent clause-map]
  (let [clause (:element clause-map)]
    (if (= (n/tag clause)
           :list)
      ;; Format :require/:import clauses specially
      (let [children (n/children clause)
            cleaned (group-with-prefixes-and-comments children)]
        (if (empty? cleaned)
          clause
          (let [[first-elem & rest-elems] cleaned
                first-node (:element first-elem)]
            (cond
              ;; :require clause - keep vectors intact, sort alphabetically
              (and (is-token? first-node)
                   (= (n/sexpr first-node)
                      :require))
              (format-require-clause first-elem rest-elems indent)

              ;; :import clause - sort packages and format specially
              (and (is-token? first-node)
                   (= (n/sexpr first-node)
                      :import))
              (format-import-clause first-elem rest-elems indent)

              ;; Other clauses - format normally
              :else
              (format-node (+ indent 2) clause)))))
      ;; Non-list clauses (shouldn't happen in ns form)
      (format-node (+ indent 2) clause))))

(defn format-ns
  "Formats ns forms with special handling for :require and :import clauses"
  [form-type args indent]
  (let [[ns-name-map & clauses] args
        ns-name (:element ns-name-map)
        separator (make-separator (+ indent 2))

        ;; Helper to get clause type
        get-clause-type (fn [clause-map]
                          (let [elem (:element clause-map)]
                            (when (= (n/tag elem)
                                     :list)
                              (let [children (n/children elem)
                                    first-child (first (remove #(strippable? (n/tag %))
                                                               children))]
                                (when (and (some? first-child)
                                           (= (n/tag first-child)
                                              :token))
                                  (n/sexpr first-child))))))

        {require-clauses :require
         import-clauses :import
         other-clauses :other} (reduce (fn [acc clause-map]
                                         (case (get-clause-type clause-map)
                                           :require (update acc :require conj clause-map)
                                           :import (update acc :import conj clause-map)
                                           (update acc :other conj clause-map)))
                                       {:require []
                                        :import []
                                        :other []}
                                       clauses)

        ;; Format each group
        formatted-other (map (partial format-ns-clause indent)
                             other-clauses)
        formatted-requires (map (partial format-ns-clause indent)
                                require-clauses)
        formatted-imports (map (partial format-ns-clause indent)
                               import-clauses)

        ;; Combine in the correct order: other clauses, requires, imports
        ordered-clauses (concat formatted-other
                                formatted-requires
                                formatted-imports)]

    ;; Build the ns form with name on same line and clauses indented
    (-> (concat [form-type
                 (n/spaces 1)
                 ns-name]
                (when (seq ordered-clauses)
                  separator)
                (when (seq ordered-clauses)
                  (interpose separator ordered-clauses)))
        flatten
        n/list-node)))

;; Special form dispatch map
(def special-form-handlers
  "Maps special form symbols to their formatting handler functions."
  {'ns format-ns
   'def format-def
   'defn format-function-form
   'defn- format-function-form
   'defmacro format-function-form
   'defmulti format-defmulti
   'defmethod format-defmethod
   'fn format-function-form
   'let format-binding-form
   'for format-binding-form
   'doseq format-binding-form
   'loop format-binding-form
   'binding format-binding-form
   'with-open format-binding-form
   'if-let format-binding-form
   'when-let format-binding-form
   'dotimes format-binding-form
   'if format-conditional-form
   'when format-conditional-form
   'when-not format-conditional-form
   'if-not format-conditional-form
   'cond format-pair-based-form
   'case format-pair-based-form
   'condp format-pair-based-form
   'cond-> format-pair-based-form
   'cond->> format-pair-based-form
   'try format-try-form
   'do format-body-only-form
   'comment format-body-only-form})

;; Default - return as-is
(defmethod format-node :default [indent node]
  node)

;; Comment formatting - transform single semicolon to double
(defmethod format-node :comment [indent node]
  (let [comment-text (n/string node)
        ;; Remove trailing whitespace
        trimmed-text (str/trimr comment-text)
        ;; Pattern: semicolon at start NOT followed by another semicolon
        normalized-text (str/replace trimmed-text #"^;(?!;)" ";;")]
    (-> normalized-text
        (subs 1)
        n/comment-node)))

;; Vector formatting
(defmethod format-node :vector [indent node]
  (let [children (n/children node)
        grouped (group-with-prefixes-and-comments children)]
    (if (empty? grouped)
      (n/vector-node [])
      (let [;; Format each element
            formatted (map (partial format-element-with-prefix-and-comment
                                    (+ indent 1))
                           grouped)

            ;; Add separator between elements
            separator (make-separator (+ indent 1))
            elements-with-separators (interpose separator formatted)

            ;; Add closing bracket spacing if needed
            final-elements (add-closing-bracket-spacing elements-with-separators
                                                        (+ indent 1))]
        (->> final-elements
             flatten
             n/vector-node)))))

;; Map formatting
(defmethod format-node :map [indent node]
  (->> (n/children node)
       group-with-prefixes-and-comments
       build-pairs
       (format-pair-entries (+ indent 1))
       (interpose (make-separator (+ indent 1)))
       flatten
       n/map-node))

;; Set formatting
(defmethod format-node :set [indent node]
  (let [children (n/children node)
        grouped (group-with-prefixes-and-comments children)
        formatted (map (partial format-element-with-prefix-and-comment (+ indent 2))
                       grouped)
        separator (make-separator (+ indent 2))
        elements-with-separators (interpose separator formatted)
        final-elements (add-closing-bracket-spacing elements-with-separators
                                                    (+ indent 2))]
    (->> final-elements
         flatten
         n/set-node)))

;; Quote formatting
(defmethod format-node :quote [indent node]
  (let [children (n/children node)]
    (if (empty? children)
      node
      (let [inner-expr (first children)
            ;; Quote adds 1 character (')
            formatted-inner (format-node (+ indent 1) inner-expr)]
        (n/quote-node formatted-inner)))))

;; Syntax-quote formatting
(defmethod format-node :syntax-quote [indent node]
  (let [children (n/children node)]
    (if (empty? children)
      node
      (let [inner-expr (first children)
            ;; Syntax-quote adds 1 character (`)
            formatted-inner (format-node (+ indent 1) inner-expr)]
        (n/syntax-quote-node formatted-inner)))))

;; Unquote formatting (~x)
(defmethod format-node :unquote [indent node]
  (let [children (n/children node)]
    (if (empty? children)
      node
      (let [inner-expr (first children)
            ;; Unquote adds 1 character (~)
            formatted-inner (format-node (+ indent 1) inner-expr)]
        (n/unquote-node formatted-inner)))))

;; Unquote-splicing formatting (~@x)
(defmethod format-node :unquote-splicing [indent node]
  (let [children (n/children node)]
    (if (empty? children)
      node
      (let [inner-expr (first children)
            ;; Unquote-splicing adds 2 characters (~@)
            formatted-inner (format-node (+ indent 2) inner-expr)]
        (n/unquote-splicing-node formatted-inner)))))

;; Var formatting (#'x)
(defmethod format-node :var [indent node]
  (let [children (n/children node)]
    (if (empty? children)
      node
      (let [inner-expr (first children)
            ;; Var adds 2 characters (#')
            formatted-inner (format-node (+ indent 2) inner-expr)]
        (n/var-node formatted-inner)))))

(defmethod format-node :fn [indent node]
  ;; #(...) anonymous function form
  ;; Format as a list but adjust indent since #( is 2 chars but list formatting adds 1 for (
  (let [list-node (n/list-node (n/children node))
        ;; The list formatter will add 1 for the opening paren
        ;; We need total of indent + 2, so pass indent + 1
        formatted-list (format-node (+ indent 1) list-node)]
    ;; Extract children from formatted list and wrap in fn-node
    (n/fn-node (n/children formatted-list))))

(defn- flatten-stacked-unevals
  "Recursively flattens nested :uneval nodes.
   #_ #_ x y -> [#_x #_y]
   Returns a sequence of nodes where nested unevals are flattened."
  [node]
  (if (= (n/tag node)
         :uneval)
    ;; It's an uneval - check its children
    (let [children (n/children node)
          non-ws (remove (comp strippable? n/tag)
                         children)]
      (if (and (seq non-ws)
               (= (-> non-ws first n/tag)
                  :uneval))
        ;; Nested uneval case - recursively flatten
        (let [[inner-uneval & remaining-in-outer] non-ws
              ;; Recursively flatten the inner uneval
              flattened-inner (flatten-stacked-unevals inner-uneval)
              ;; Each remaining form in outer uneval gets its own #_
              flattened-remaining (map n/uneval-node remaining-in-outer)]
          ;; Combine all the flattened unevals
          (concat flattened-inner flattened-remaining))
        ;; Simple uneval - return as-is
        [node]))
    ;; Not an uneval - return as-is
    [node]))

(defmethod format-node :uneval [indent node]
  ;; #_ reader macro (comment out form)
  (let [children (n/children node)]
    (if (empty? children)
      node
      (let [non-whitespace (remove #(strippable? (n/tag %)) children)]
        (if (and (seq non-whitespace)
                 (= (n/tag (first non-whitespace))
                    :uneval))
          ;; Stacked #_ case - flatten and return with newlines
          (let [flattened (flatten-stacked-unevals node)
                formatted-flattened (map #(format-node indent %) flattened)]
            (if (= (count formatted-flattened)
                   1)
              (first formatted-flattened)
              ;; Multiple flattened unevals - separate with newlines and parent indent
              (let [separator (if (> indent 0)
                                [(n/newlines 1) (n/spaces indent)]
                                [(n/newlines 1)])]
                (->> formatted-flattened
                     (interpose separator)
                     flatten
                     n/forms-node))))
          ;; Simple case: single form being unevaled
          (let [inner-expr (first non-whitespace)
                ;; #_ adds 2 characters
                formatted-inner (format-node (+ indent 2) inner-expr)]
            (n/uneval-node formatted-inner)))))))

(defmethod format-node :forms [indent node]
  (let [children (try (n/children node) (catch Exception _ []))
        grouped (group-with-prefixes-and-comments children)
        is-comment? (fn [item]
                      (= (n/tag item)
                         :comment))]
    (if (empty? grouped)
      (n/forms-node [])
      (let [formatted-items (map (partial format-element-with-prefix-and-comment
                                          indent)
                                 grouped)]
        (if (= (count formatted-items) 1)
          (first formatted-items)
          ;; Use reduce to build items with proper separators
          (->> formatted-items
               (reduce (fn [{:keys [result prev-item]} current-item]
                         (let [prev-was-comment? (and (some? prev-item)
                                                      (comment? prev-item))
                               current-is-comment? (comment? current-item)
                               ;; Separator rules:
                               ;; - Comment-to-anything: 1 newline
                               ;; - Form-to-comment: 2 newlines (blank line before comment)
                               ;; - Form-to-form: 2 newlines
                               separator (cond
                                           prev-was-comment? (n/newlines 1)
                                           current-is-comment? (n/newlines 2)
                                           :else (n/newlines 2))]
                           {:result (if (some? prev-item)
                                      (conj result separator current-item)
                                      (conj result current-item))
                            :prev-item current-item}))
                       {:result []
                        :prev-item nil})
               :result
               vec
               n/forms-node))))))

(defn- format-function-call
  "Format a function call with aligned arguments."
  [formatted-function grouped-args indent]
  (let [function-string (n/string formatted-function)
        {:keys [multi-line? last-line]} (analyze-multiline function-string)
        arg-align-pos (if multi-line?
                        (+ (count last-line) 1)
                        (+ indent 1 (count last-line) 1))
        formatted-args (map (partial format-element-with-prefix-and-comment
                                     arg-align-pos)
                            grouped-args)]
    (if (empty? formatted-args)
      ;; No arguments case - just use add-closing-bracket-spacing
      (let [elements [formatted-function]
            final-elements (add-closing-bracket-spacing elements (+ indent 1))]
        (n/list-node (flatten final-elements)))
      ;; Has arguments
      (let [first-line [formatted-function (n/spaces 1) (first formatted-args)]
            separator (make-separator arg-align-pos)
            rest-formatted (rest formatted-args)]
        (if (empty? rest-formatted)
          (n/list-node (flatten first-line))
          (let [elements-with-separators (concat first-line
                                                 separator
                                                 (interpose separator rest-formatted))
                final-elements (add-closing-bracket-spacing elements-with-separators
                                                            arg-align-pos)]
            (n/list-node (flatten final-elements))))))))

;; List formatting
(defmethod format-node :list [indent node]
  (let [children (n/children node)
        cleaned (group-with-prefixes-and-comments children)]
    (if (empty? cleaned)
      (n/list-node [])
      (let [format-child (partial format-node indent)
            [first-element & rest-elements] cleaned
            ;; Check if first element has inline comment
            [actual-first-element inline-comment-after-fn]
            (if (:comment first-element)
              [(:element first-element) (:comment first-element)]
              [(:element first-element) nil])

            ;; Check for comments between function name and first argument
            [leading-comment-maps actual-args] (extract-leading-comments rest-elements)
            leading-comments (map :element leading-comment-maps)

            ;; Combine any inline comment after function name with leading comments
            all-leading-comments (if (some? inline-comment-after-fn)
                                   (cons inline-comment-after-fn leading-comments)
                                   leading-comments)

            ;; Format the function element including any inline comment
            formatted-function (format-element-with-prefix-and-comment (+ indent 1)
                                                                       first-element)
            function-symbol (when (is-token? actual-first-element)
                              (n/sexpr actual-first-element))

            ;; Forms that handle their own comment formatting
            comment-handling-forms #{'do 'comment 'cond 'case 'condp 'cond-> 'cond->>}]
        (cond
          ;; Comments between function name and first arg - move them before the form
          ;; (except for forms that handle their own comments)
          (and (seq all-leading-comments)
               (seq actual-args)
               (not (contains? comment-handling-forms function-symbol)))
          (let [formatted-comments (map (partial format-node indent) all-leading-comments)
                ;; Skip past comments and whitespace to get to the actual arguments
                children-after-leading (drop-while (fn [node]
                                                     (let [tag (n/tag node)]
                                                       (or (= tag :comment)
                                                           (strippable? tag))))
                                                   (rest children))
                ;; Reconstruct the list without the leading comments
                new-list-node (-> (cons actual-first-element children-after-leading)
                                  n/list-node
                                  format-child)]
            (n/forms-node (concat formatted-comments
                                  [(n/newlines 1) new-list-node])))

          ;; Comments but no arguments - move comments before the form
          ;; (except for forms that handle their own comments)
          (and (seq all-leading-comments)
               (empty? actual-args)
               (not (contains? comment-handling-forms function-symbol)))
          (let [formatted-comments (map (partial format-node indent) all-leading-comments)
                ;; Just the function name in parentheses
                simple-function (format-node (+ indent 1) actual-first-element)
                new-list-node (n/list-node [simple-function])]
            (n/forms-node (concat formatted-comments
                                  [(n/newlines 1) new-list-node])))

          ;; Special form handler exists
          (contains? special-form-handlers function-symbol)
          (let [handler (get special-form-handlers function-symbol)
                ;; For forms that handle comments, include all comments as args
                args-to-pass (if (contains? comment-handling-forms function-symbol)
                               (concat (when (some? inline-comment-after-fn)
                                         [{:element inline-comment-after-fn}])
                                       leading-comment-maps
                                       actual-args)
                               actual-args)]
            (handler actual-first-element args-to-pass indent))

          ;; Regular function call - pass the grouped args directly
          :else
          (format-function-call formatted-function actual-args indent))))))
