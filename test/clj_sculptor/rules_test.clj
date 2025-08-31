(ns clj-sculptor.rules-test
  (:require [clj-sculptor.core :as core]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(defn lines
  "Helper function to create multi-line strings from individual lines."
  [& lines]
  (str/join "\n" lines))

(deftest test-trailing-whitespace-rule
  (testing "when lines have trailing spaces"
    (let [code-with-trailing (lines "(defn foo []  "
                                    "  :bar)  ")
          expected (lines "(defn foo []"
                          "  :bar)")
          formatted (core/format-code code-with-trailing)]
      (is (= expected formatted))))
  (testing "when code has no trailing whitespace"
    (let [clean-code (lines "(defn foo []"
                            "  :bar)")
          formatted (core/format-code clean-code)]
      (is (= clean-code formatted))))
  (testing "when multiple lines have various amounts of trailing spaces"
    (let [code (lines "  (def x 1)   "
                      "  (def y 2)  ")
          expected (lines "(def x"
                          "  1)"
                          ""
                          "(def y"
                          "  2)")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-leading-whitespace-rule
  (testing "when document starts with whitespace"
    (let [code "  (def x 1)"
          expected "(def x\n  1)"
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-2-space-indentation-rule
  (testing "when code has 4-space indentation"
    (let [code (lines "(defn foo [x]"
                      "    (+ x 1))")
          expected (lines "(defn foo [x]"
                          "  (+ x"
                          "     1))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when code has 8-space indentation"
    (let [code (lines "(defn bar [y]"
                      "        (* y 2))")
          expected (lines "(defn bar [y]"
                          "  (* y"
                          "     2))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when code already has 2-space indentation"
    (let [code (lines "(defn baz [z]"
                      "  (- z 1))")
          expected (lines "(defn baz [z]"
                          "  (- z"
                          "     1))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when code has nested forms with inconsistent indentation"
    (let [code (lines "(defn nested [x]"
                      "    (if (> x 0)"
                      "        (inc x)"
                      "        (dec x)))")
          ;; AST-aware indentation: 2 spaces for defn body, 4 spaces for if branches
          expected (lines "(defn nested [x]"
                          "  (if (> x"
                          "         0)"
                          "    (inc x)"
                          "    (dec x)))")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-context-aware-indentation
  (testing "when vector elements have inconsistent spacing"
    (let [code (lines "[1"
                      "  2"
                      "   3]")
          expected (lines "[1"
                          " 2"
                          " 3]")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when map entries have inconsistent alignment"
    (let [code (lines "{:a 1"
                      "   :b 2"
                      "    :c 3}")
          expected (lines "{:a 1"
                          " :b 2"
                          " :c 3}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when map keys are complex structures"
    (let [code (lines "{[:complex"
                      "   :multi"
                      "  :line"
                      "   :key] [\"test\""
                      "                \"alignment\"] }")
          expected (lines "{[:complex"
                          "  :multi"
                          "  :line"
                          "  :key] [\"test\""
                          "         \"alignment\"]}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when complex map keys are nested one level deep"
    (let [code (lines "{:data {[:complex"
                      "            :multi"
                      "           :line"
                      "            :key] [\"test\""
                      "                         \"alignment\"]}}")
          expected (lines "{:data {[:complex"
                          "         :multi"
                          "         :line"
                          "         :key] [\"test\""
                          "                \"alignment\"]}}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when set elements have varied indentation"
    (let [code (lines "#{:a"
                      "   :b"
                      "    :c}")
          expected (lines "#{:a"
                          "  :b"
                          "  :c}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when nested collections have misaligned elements"
    (let [code (lines "{:values [1"
                      "             2"
                      "             3]"
                      "    :other {:nested [4"
                      "                       5]}}")
          expected (lines "{:values [1"
                          "          2"
                          "          3]"
                          " :other {:nested [4"
                          "                  5]}}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when vector contains function definitions with wrong indentation"
    (let [code (lines "[(defn foo [x]"
                      "      (+ x 1))"
                      "    (defn bar [y]"
                      "      (* y 2))]")
          expected (lines "[(defn foo [x]"
                          "   (+ x"
                          "      1))"
                          " (defn bar [y]"
                          "   (* y"
                          "      2))]")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when deeply nested collections have complex mixed structures"
    (let [code (str "{:config {:database {:host \"localhost\" :port 5432 "
                    ":ssl {:enabled true :cert \"/path/cert.pem\"}} "
                    ":cache {:ttl 300 :backends #{:redis :memory}}} "
                    ":data {:users [{:id 1 :roles #{:admin :user} "
                    ":settings {:theme \"dark\" :notifications {:email true :push false}}} "
                    "{:id 2 :roles #{:user} :metadata {:tags [\"important\" \"customer\"] "
                    ":scores {:total 95 :breakdown {:performance 98 :reliability 92}}}}] "
                    "[:complex :multi :line :key] [\"test\" \"alignment\"] "
                    ":stats {:daily [100 150 200] :weekly #{:mon :tue :wed}}}}")
          expected (lines "{:config {:database {:host \"localhost\""
                          "                     :port 5432"
                          "                     :ssl {:enabled true"
                          "                           :cert \"/path/cert.pem\"}}"
                          "          :cache {:ttl 300"
                          "                  :backends #{:redis"
                          "                              :memory}}}"
                          " :data {:users [{:id 1"
                          "                 :roles #{:admin"
                          "                          :user}"
                          "                 :settings {:theme \"dark\""
                          "                            :notifications {:email true"
                          "                                            :push false}}}"
                          "                {:id 2"
                          "                 :roles #{:user}"
                          "                 :metadata {:tags [\"important\""
                          "                                   \"customer\"]"
                          "                            :scores {:total 95"
                          "                                     :breakdown {:performance 98"
                          "                                                 :reliability 92}}}}]"
                          "        [:complex"
                          "         :multi"
                          "         :line"
                          "         :key] [\"test\""
                          "                \"alignment\"]"
                          "        :stats {:daily [100"
                          "                        150"
                          "                        200]"
                          "                :weekly #{:mon"
                          "                          :tue"
                          "                          :wed}}}}")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Deep nesting should maintain proper alignment throughout"))))

(deftest test-newline-indentation-insertion
  (testing "when newline is not followed by whitespace"
    (let [code "{:a 1\n:b 2}"
          expected "{:a 1\n :b 2}"
          formatted (core/format-code code)]
      (is (= expected formatted) "Should insert 1 space for map alignment")))
  (testing "when vector elements have no indentation after newline"
    (let [code "[1\n2\n3]"
          expected "[1\n 2\n 3]"
          formatted (core/format-code code)]
      (is (= expected formatted) "Should insert 1 space for vector alignment")))
  (testing "when set elements have no indentation after newline"
    (let [code "#{1\n2\n3}"
          expected "#{1\n  2\n  3}"
          formatted (core/format-code code)]
      (is (= expected formatted) "Should insert 2 spaces for set alignment")))
  (testing "when function body has no indentation after newline"
    (let [code "(defn foo []\n:bar)"
          expected "(defn foo []\n  :bar)"
          formatted (core/format-code code)]
      (is (= expected formatted) "Should insert 2 spaces for function body")))
  (testing "when nested map has no indentation"
    (let [code "{:outer {:inner 1\n:key 2}}"
          expected "{:outer {:inner 1\n         :key 2}}"
          formatted (core/format-code code)]
      (is (= expected formatted) "Should align nested map keys with position tracking"))))

(deftest test-function-argument-alignment
  (testing "when function arguments are already on same line with first arg"
    (let [code (lines "(some-function arg1"
                      "  arg2"
                      "     arg3)")
          expected (lines "(some-function arg1"
                          "               arg2"
                          "               arg3)")
          formatted (core/format-code code)]
      (is (= expected formatted) "Arguments should align with first argument")))
  (testing "when first argument is on next line after function name"
    (let [code (lines "(some-function"
                      "arg1"
                      "   arg2"
                      " arg3)")
          expected (lines "(some-function arg1"
                          "               arg2"
                          "               arg3)")
          formatted (core/format-code code)]
      (is (= expected formatted) "First arg should move to same line, others should align")))
  (testing "when function has multiple args on first line then more on subsequent lines"
    (let [code (lines "(function arg1 arg2"
                      "  arg3"
                      "     arg4)")
          expected (lines "(function arg1"
                          "          arg2"
                          "          arg3"
                          "          arg4)")
          formatted (core/format-code code)]
      (is (= expected formatted) "Only first arg stays on same line, others align below")))
  (testing "when function call is nested inside let binding"
    (let [code (lines "(let [x (some-fn arg1"
                      "           arg2)]"
                      "  x)")
          expected (lines "(let [x (some-fn arg1"
                          "                 arg2)]"
                          "  x)")
          formatted (core/format-code code)]
      (is (= expected formatted) "Nested function calls should maintain proper alignment")))
  (testing "when first argument has extra indentation on next line"
    (let [code (lines "(func"
                      "   arg1"
                      "   arg2)")
          expected (lines "(func arg1"
                          "      arg2)")
          formatted (core/format-code code)]
      (is (= expected formatted) "First arg should move to same line")))
  (testing "when keyword is used as function"
    (let [code "(:key some-map other-arg)"
          expected (lines "(:key some-map"
                          "      other-arg)")
          formatted (core/format-code code)]
      (is (= expected formatted) "Keyword function arguments should align properly")))
  (testing "when multi-line function expression is used"
    (let [code "((comp inc dec) x y)"
          expected (lines "((comp inc"
                          "       dec) x"
                          "            y)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-line function expression arguments should align with closing paren"))))

(deftest test-multiple-top-level-forms
  (testing "when file contains multiple varied top-level forms"
    (let [code (str "(defn helper [x] (+ x 1)) "
                    "(def config {:key \"value\"}) "
                    "(defmulti multi-fn :type) "
                    "(defmethod multi-fn :type [{:keys [data]}] (process data)) "
                    "(defmulti complex-dispatch (fn [x] (if (map? x) :map :other)))")
          expected (lines "(defn helper [x]"
                          "  (+ x"
                          "     1))"
                          ""
                          "(def config\n  {:key \"value\"})"
                          ""
                          "(defmulti multi-fn"
                          "  :type)"
                          ""
                          "(defmethod multi-fn :type [{:keys [data]}]"
                          "  (process data))"
                          ""
                          "(defmulti complex-dispatch"
                          "  (fn [x]"
                          "    (if (map? x)"
                          "      :map"
                          "      :other)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multiple top-level forms should be properly separated and formatted")))
  (testing "when multiple forms have inconsistent spacing between them"
    (let [code "(def x 1)   (def y 2)    (defn add [a b] (+ a b))"
          expected (lines "(def x\n  1)"
                          ""
                          "(def y\n  2)"
                          ""
                          "(defn add [a"
                          "           b]"
                          "  (+ a"
                          "     b))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Forms should be separated by newlines with consistent formatting")))
  (testing "when forms contain complex nested structures"
    (let [code (str "(def data [{:users [{:name \"Alice\" :roles #{:admin :user}}]} "
                    "{:system {:config {:db {:host \"localhost\"}}}}]) "
                    "(defn simple [input] (process input))")
          expected (lines "(def data"
                          "  [{:users [{:name \"Alice\""
                          "             :roles #{:admin"
                          "                      :user}}]}"
                          "   {:system {:config {:db {:host \"localhost\"}}}}])"
                          ""
                          "(defn simple [input]"
                          "  (process input))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex nested structures in multiple forms should maintain proper alignment"))))

(deftest test-require-formatting
  (testing "when ns form is empty"
    (let [code "(ns  example)"
          expected "(ns example)"
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when ns form has single require"
    (let [code "(ns example (:require [clojure.string :as str]))"
          expected (lines "(ns example"
                          "  (:require [clojure.string :as str]))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Single require should be formatted with proper indentation")))
  (testing "when ns form has multiple requires"
    (let [code (str "(ns example (:require [clojure.string :as str] "
                    "[clojure.set :as set] [clojure.walk :as walk]))")
          expected (lines "(ns example"
                          "  (:require [clojure.set :as set]"
                          "            [clojure.string :as str]"
                          "            [clojure.walk :as walk]))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          (str "Multiple requires should align like function arguments "
               "and be sorted alphabetically"))))
  (testing "when require list spans multiple lines"
    (let [code (lines "(ns example"
                      "  (:require [clojure.string :as str]"
                      "            [clojure.set :as set]))")
          formatted (core/format-code code)]
      (is (= (lines "(ns example"
                    "  (:require [clojure.set :as set]"
                    "            [clojure.string :as str]))")
             formatted)
          "Multi-line require should maintain alignment and be sorted alphabetically"))))

(deftest test-import-formatting
  (testing "when ns form has single import"
    (let [code "(ns example (:import (java.util Date)))"
          expected (lines "(ns example"
                          "  (:import (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Single import should be formatted with proper indentation")))
  (testing "when ns form has multiple imports"
    (let [code "(ns example (:import (java.util Date Calendar) (java.io File FileReader)))"
          expected (lines "(ns example"
                          "  (:import (java.io File"
                          "                    FileReader)"
                          "           (java.util Date"
                          "                      Calendar)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          (str "Multiple import lists should align like function arguments "
               "and be sorted alphabetically"))))
  (testing "when import uses list with multiple classes"
    (let [code "(ns example (:import (java.util Date Calendar TimeZone)))"
          expected (lines "(ns example"
                          "  (:import (java.util Date"
                          "                      Calendar"
                          "                      TimeZone)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Import list with multiple classes should maintain proper formatting")))
  (testing "when ns has require with multiple entries and import"
    (let [code (str "(ns example (:require [clojure.string :as str] [clojure.set :as set]) "
                    "(:import (java.util Date) (java.io File)))")
          expected (lines "(ns example"
                          "  (:require [clojure.set :as set]"
                          "            [clojure.string :as str])"
                          "  (:import (java.io File)"
                          "           (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          (str "Multiple requires and imports should align within their respective forms "
               "and be sorted alphabetically")))))

(deftest test-collection-type-normalization
  (testing "when :require uses wrong list syntax"
    (let [code "(ns example (:require (clojure.string :as str)))"
          expected (lines "(ns example"
                          "  (:require [clojure.string :as str]))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Lists in :require should be converted to vectors")))
  (testing "when :import uses wrong vector syntax"
    (let [code "(ns example (:import [java.util Date]))"
          expected (lines "(ns example"
                          "  (:import (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Vectors in :import should be converted to lists")))
  (testing "when both :require and :import use wrong syntax"
    (let [code "(ns example (:require (clojure.string :as str)) (:import [java.util Date]))"
          expected (lines "(ns example"
                          "  (:require [clojure.string :as str])"
                          "  (:import (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Both wrong syntaxes should be corrected simultaneously")))
  (testing "when multiple require lists need conversion"
    (let [code "(ns example (:require (clojure.string :as str) (clojure.set :as set)))"
          expected (lines "(ns example"
                          "  (:require [clojure.set :as set]"
                          "            [clojure.string :as str]))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          (str "Multiple lists in :require should all be converted to vectors "
               "and sorted alphabetically"))))
  (testing "when multiple import vectors need conversion"
    (let [code "(ns example (:import [java.util Date] [java.io File]))"
          expected (lines "(ns example"
                          "  (:import (java.io File)"
                          "           (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          (str "Multiple vectors in :import should all be converted to lists "
               "and sorted alphabetically")))))

(deftest test-namespace-ordering
  (testing "when :import comes before :require"
    (let [code "(ns example (:import (java.util Date)) (:require [clojure.string :as str]))"
          expected (lines "(ns example"
                          "  (:require [clojure.string :as str])"
                          "  (:import (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          ":require should come before :import")))
  (testing "when namespaces are not sorted alphabetically in :require"
    (let [code "(ns example (:require [clojure.string :as str] [clojure.core :as core]))"
          expected (lines "(ns example"
                          "  (:require [clojure.core :as core]"
                          "            [clojure.string :as str]))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Namespaces in :require should be sorted alphabetically")))
  (testing "when namespaces are not sorted alphabetically in :import"
    (let [code "(ns example (:import (java.util Date) (java.io File)))"
          expected (lines "(ns example"
                          "  (:import (java.io File)"
                          "           (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Import specs should be sorted alphabetically")))
  (testing "when both ordering and sorting are needed"
    (let [code (str "(ns example (:import (java.util Date) (java.io File)) "
                    "(:require [clojure.string :as str] [clojure.core :as core]))")
          expected (lines "(ns example"
                          "  (:require [clojure.core :as core]"
                          "            [clojure.string :as str])"
                          "  (:import (java.io File)"
                          "           (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Should reorder and sort both :require and :import")))
  (testing "when namespace already correctly ordered and sorted"
    (let [code (lines "(ns example"
                      "  (:require [clojure.core :as core]"
                      "            [clojure.string :as str])"
                      "  (:import (java.io File)"
                      "           (java.util Date)))")
          formatted (core/format-code code)]
      (is (= code formatted)
          "Already correct ordering and sorting should be preserved")))
  (testing "when namespace has docstring with imports and requires"
    (let [code (str "(ns example \"A namespace with a docstring\" "
                    "(:import (java.util Date)) (:require [clojure.string :as str]))")
          expected (lines "(ns example"
                          "  \"A namespace with a docstring\""
                          "  (:require [clojure.string :as str])"
                          "  (:import (java.util Date)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Docstring should come after ns name, then :require before :import"))))

(deftest test-def-formatting
  (testing "when def has simple value"
    (let [code "(def simple-var 42)"
          expected (lines "(def simple-var"
                          "  42)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Simple def values should go on separate line")))
  (testing "when def has complex value"
    (let [code "(def complex-var {:a 1 :b 2})"
          expected (lines "(def complex-var"
                          "  {:a 1"
                          "   :b 2})")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex def values should go on separate line and be properly formatted")))
  (testing "when def has docstring and value"
    (let [code "(def var-with-doc \"Documentation string\" 42)"
          expected (lines "(def var-with-doc"
                          "  \"Documentation string\""
                          "  42)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Def docstring should go on separate line between name and value")))
  (testing "when def has docstring and complex value"
    (let [code "(def documented-map \"A map with documentation\" {:key :value})"
          expected (lines "(def documented-map"
                          "  \"A map with documentation\""
                          "  {:key :value})")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Def with docstring and complex value should format correctly")))
  (testing "when def has vector value"
    (let [code "(def my-vector [1 2 3])"
          expected (lines "(def my-vector"
                          "  [1"
                          "   2"
                          "   3])")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Def with vector value should format vector on separate line")))
  (testing "when def has function value"
    (let [code "(def my-fn (fn [x] (+ x 1)))"
          expected (lines "(def my-fn"
                          "  (fn [x]"
                          "    (+ x"
                          "       1)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Def with function value should format function on separate line"))))

(deftest test-defn-formatting
  (testing "when defn has no docstring"
    (let [code "(defn simple-fn [x] (+ x 1))"
          expected (lines "(defn simple-fn [x]"
                          "  (+ x"
                          "     1))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Args vector should stay on same line when no docstring")))
  (testing "when defn has docstring"
    (let [code "(defn documented-fn \"This adds one\" [x] (+ x 1))"
          expected (lines "(defn documented-fn"
                          "  \"This adds one\""
                          "  [x]"
                          "  (+ x"
                          "     1))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Docstring should go on next line, args vector after docstring")))
  (testing "when defn has multi-line docstring"
    (let [code (lines "(defn multi-doc-fn \"Line one"
                      "Line two\" [x] x)")
          expected (lines "(defn multi-doc-fn"
                          "  \"Line one"
                          "Line two\""
                          "  [x]"
                          "  x)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-line docstring should be preserved with args on next line")))
  (testing "when defn has multiple args"
    (let [code "(defn multi-arg [a b c] (+ a b c))"
          expected (lines "(defn multi-arg [a"
                          "                 b"
                          "                 c]"
                          "  (+ a"
                          "     b"
                          "     c))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multiple args should align properly")))
  (testing "when defn has docstring and multiple args"
    (let [code "(defn doc-multi-arg \"Adds three numbers\" [a b c] (+ a b c))"
          expected (lines "(defn doc-multi-arg"
                          "  \"Adds three numbers\""
                          "  [a"
                          "   b"
                          "   c]"
                          "  (+ a"
                          "     b"
                          "     c))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Docstring and multiple args should format correctly")))
  (testing "when defn has destructuring"
    (let [code "(defn destructure-fn [{:keys [a b]}] (+ a b))"
          expected (lines "(defn destructure-fn [{:keys [a"
                          "                              b]}]"
                          "  (+ a"
                          "     b))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Destructuring in args should format correctly")))
  (testing "when defn has docstring and destructuring"
    (let [code "(defn doc-destructure \"Destructures map\" [{:keys [x y]}] (+ x y))"
          expected (lines "(defn doc-destructure"
                          "  \"Destructures map\""
                          "  [{:keys [x"
                          "           y]}]"
                          "  (+ x"
                          "     y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Docstring with destructuring should format correctly")))
  (testing "when defn has variadic args"
    (let [code "(defn variadic-fn [a & rest] (apply + a rest))"
          expected (lines "(defn variadic-fn [a"
                          "                   & rest]"
                          "  (apply +"
                          "         a"
                          "         rest))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Variadic args should format correctly")))
  (testing "when defn has complex destructuring after &"
    (let [code "(defn f [x & {:keys [a b c]}] (+ x a b c))"
          expected (lines "(defn f [x"
                          "         & {:keys [a"
                          "                   b"
                          "                   c]}]"
                          "  (+ x"
                          "     a"
                          "     b"
                          "     c))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex destructuring after & should align properly")))
  (testing "when defn has vector destructuring after &"
    (let [code "(defn g [x & [first second]] (+ x first second))"
          expected (lines "(defn g [x"
                          "         & [first"
                          "            second]]"
                          "  (+ x"
                          "     first"
                          "     second))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Vector destructuring after & should align properly")))
  (testing "when defn has docstring and variadic args"
    (let [code "(defn doc-variadic \"Sums all args\" [a & rest] (apply + a rest))"
          expected (lines "(defn doc-variadic"
                          "  \"Sums all args\""
                          "  [a"
                          "   & rest]"
                          "  (apply +"
                          "         a"
                          "         rest))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Docstring with variadic args should format correctly")))
  (testing "when defn- (private) has docstring"
    (let [code "(defn- private-fn \"Private function\" [x] x)"
          expected (lines "(defn- private-fn"
                          "  \"Private function\""
                          "  [x]"
                          "  x)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Private defn with docstring should format like public defn")))
  (testing "when defn has multiple arities without docstring"
    (let [code "(defn multi-arity ([x] x) ([x y] (+ x y)))"
          expected (lines "(defn multi-arity"
                          "  ([x]"
                          "   x)"
                          "  ([x"
                          "    y]"
                          "   (+ x"
                          "      y)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-arity defn should format each arity on separate lines")))
  (testing "when defn has multiple arities with docstring"
    (let [code "(defn sum \"Sums numbers\" ([x] x) ([x y] (+ x y)) ([x y z] (+ x y z)))"
          expected (lines "(defn sum"
                          "  \"Sums numbers\""
                          "  ([x]"
                          "   x)"
                          "  ([x"
                          "    y]"
                          "   (+ x"
                          "      y))"
                          "  ([x"
                          "    y"
                          "    z]"
                          "   (+ x"
                          "      y"
                          "      z)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-arity defn with docstring should format correctly")))
  (testing "when defn- has multiple arities"
    (let [code "(defn- private-multi ([x] x) ([x y] (+ x y)))"
          expected (lines "(defn- private-multi"
                          "  ([x]"
                          "   x)"
                          "  ([x"
                          "    y]"
                          "   (+ x"
                          "      y)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Private multi-arity defn should format correctly")))
  (testing "when multi-arity defn has comments in body"
    (let [code (lines "(defn process ([x] ;; single arg"
                      "(inc x)) ([x y] ;; two args"
                      ";; block comment"
                      "(+ x y)))")
          expected (lines "(defn process"
                          "  ([x] ;; single arg"
                          "   (inc x))"
                          "  ([x"
                          "    y] ;; two args"
                          "   ;; block comment"
                          "   (+ x"
                          "      y)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-arity defn with comments between args and body should format correctly")))
  (testing "when defn has newline between & and rest with inline comment"
    (let [code (lines "(defn variadic-newline [a &"
                      "                         rest ;; variadic args"
                      "                        ]"
                      "  (apply + a rest))")
          expected (lines "(defn variadic-newline [a"
                          "                        & rest ;; variadic args"
                          "                        ]"
                          "  (apply +"
                          "         a"
                          "         rest))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          (str "Newline between & and rest with inline comment should preserve existing spacing"
               " and keep comment inline")))))

(deftest test-let-formatting
  (testing "when let has simple bindings"
    (let [code "(let [x 1 y 2] (+ x y))"
          expected (lines "(let [x 1"
                          "      y 2]"
                          "  (+ x"
                          "     y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Simple let bindings should align properly")))
  (testing "when let has complex binding values"
    (let [code (str "(let [users (get-users) filtered-users (filter active? users)] "
                    "(count filtered-users))")
          expected (lines "(let [users (get-users)"
                          "      filtered-users (filter active?"
                          "                             users)]"
                          "  (count filtered-users))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex binding values should format with proper alignment")))
  (testing "when let has multiple body expressions"
    (let [code "(let [x 1 y 2] (println x) (println y) (+ x y))"
          expected (lines "(let [x 1"
                          "      y 2]"
                          "  (println x)"
                          "  (println y)"
                          "  (+ x"
                          "     y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multiple let body expressions should align consistently")))
  (testing "when let is nested inside another let"
    (let [code "(let [x 1] (let [y (+ x 1)] (+ x y)))"
          expected (lines "(let [x 1]"
                          "  (let [y (+ x"
                          "             1)]"
                          "    (+ x"
                          "       y)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested let forms should maintain proper indentation")))
  (testing "when let has destructuring bindings"
    (let [code "(let [{:keys [a b]} data [x y] coords] (+ a b x y))"
          expected (lines "(let [{:keys [a"
                          "              b]} data"
                          "      [x"
                          "       y] coords]"
                          "  (+ a"
                          "     b"
                          "     x"
                          "     y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Destructuring bindings should format with proper alignment")))
  (testing "when let bindings contain function calls"
    (let [code (str "(let [result (calculate-value arg1 arg2) processed "
                    "(transform result)] processed)")
          expected (lines "(let [result (calculate-value arg1"
                          "                              arg2)"
                          "      processed (transform result)]"
                          "  processed)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Function calls in bindings should align properly")))
  (testing "when let is inside a function definition"
    (let [code (str "(defn process-data [input] (let [cleaned (clean input) "
                    "validated (validate cleaned)] validated))")
          expected (lines "(defn process-data [input]"
                          "  (let [cleaned (clean input)"
                          "        validated (validate cleaned)]"
                          "    validated))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Let inside function should maintain proper relative indentation")))
  (testing "when let has single binding pair"
    (let [code "(let [x 42] x)"
          expected (lines "(let [x 42]"
                          "  x)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Single binding pair should format correctly")))
  (testing "when let bindings span multiple lines"
    (let [code (lines "(let [data (fetch-data"
                      "             {:url \"http://api.example.com\""
                      "              :params {:limit 100}})"
                      "      result (process data)]"
                      "  result)")
          expected (lines "(let [data (fetch-data {:url \"http://api.example.com\""
                          "                        :params {:limit 100}})"
                          "      result (process data)]"
                          "  result)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-line binding values should format properly")))
  (testing "when let has many binding pairs"
    (let [code "(let [a 1 b 2 c 3 d 4 e 5] (+ a b c d e))"
          expected (lines "(let [a 1"
                          "      b 2"
                          "      c 3"
                          "      d 4"
                          "      e 5]"
                          "  (+ a"
                          "     b"
                          "     c"
                          "     d"
                          "     e))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Many binding pairs should maintain consistent alignment"))))

(deftest test-binding-forms
  (testing "when for has multiple bindings"
    (let [code "(for [x (range 3) y (range 2)] (* x y))"
          expected (lines "(for [x (range 3)"
                          "      y (range 2)]"
                          "  (* x"
                          "     y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "For bindings should align like let bindings")))
  (testing "when doseq has multiple bindings"
    (let [code "(doseq [x (range 3) y (range 2)] (println x y))"
          expected (lines "(doseq [x (range 3)"
                          "        y (range 2)]"
                          "  (println x"
                          "           y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Doseq bindings should align like let bindings")))
  (testing "when loop has bindings"
    (let [code "(loop [x 0 acc []] (if (< x 5) (recur (inc x) (conj acc x)) acc))"
          expected (lines "(loop [x 0"
                          "       acc []]"
                          "  (if (< x"
                          "         5)"
                          "    (recur (inc x)"
                          "           (conj acc"
                          "                 x))"
                          "    acc))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Loop bindings should align like let bindings")))
  (testing "when binding has dynamic vars"
    (let [code "(binding [*out* writer *err* error-writer] (println \"test\") (flush))"
          expected (lines "(binding [*out* writer"
                          "          *err* error-writer]"
                          "  (println \"test\")"
                          "  (flush))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Binding form should align like let bindings")))
  (testing "when with-open has resource bindings"
    (let [code "(with-open [r (reader file) w (writer output)] (copy r w))"
          expected (lines "(with-open [r (reader file)"
                          "            w (writer output)]"
                          "  (copy r"
                          "        w))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "With-open bindings should align like let bindings")))
  (testing "when if-let has condition binding"
    (let [code "(if-let [result (some-func x)] (use result) (default-value))"
          expected (lines "(if-let [result (some-func x)]"
                          "  (use result)"
                          "  (default-value))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "If-let should format with proper body indentation")))
  (testing "when when-let has condition binding"
    (let [code "(when-let [result (some-func x)] (println result) (process result))"
          expected (lines "(when-let [result (some-func x)]"
                          "  (println result)"
                          "  (process result))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "When-let should format with proper body indentation")))
  (testing "when dotimes has iteration binding"
    (let [code "(dotimes [n 5] (println n) (process n))"
          expected (lines "(dotimes [n 5]"
                          "  (println n)"
                          "  (process n))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Dotimes should format with proper body indentation"))))

(deftest test-conditional-forms
  (testing "when if has simple condition and clauses"
    (let [code "(if (> x 0) \"positive\" \"negative\")"
          expected (lines "(if (> x"
                          "       0)"
                          "  \"positive\""
                          "  \"negative\")")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "If clauses should indent 2 spaces from the if form")))
  (testing "when when has multiple body expressions"
    (let [code "(when (> x 0) (println x) (inc x) (process x))"
          expected (lines "(when (> x"
                          "         0)"
                          "  (println x)"
                          "  (inc x)"
                          "  (process x))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "When body expressions should each be on separate lines with 2-space indent")))
  (testing "when when-not has condition and body"
    (let [code "(when-not (empty? coll) (println \"not empty\") (process coll))"
          expected (lines "(when-not (empty? coll)"
                          "  (println \"not empty\")"
                          "  (process coll))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "When-not should format like when")))
  (testing "when if-not has condition and clauses"
    (let [code "(if-not (zero? x) (process x) (handle-zero))"
          expected (lines "(if-not (zero? x)"
                          "  (process x)"
                          "  (handle-zero))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "If-not should format like if")))
  (testing "when conditionals are nested"
    (let [code (str "(when (> x 0) (if (even? x) (println \"positive even\") "
                    "(println \"positive odd\")))")
          expected (lines "(when (> x"
                          "         0)"
                          "  (if (even? x)"
                          "    (println \"positive even\")"
                          "    (println \"positive odd\")))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested conditionals should maintain proper relative indentation")))
  (testing "when conditional has complex condition"
    (let [code "(if (and (pos? x) (< x 100) (not (zero? (mod x 3)))) (process x) (skip x))"
          expected (lines "(if (and (pos? x)"
                          "         (< x"
                          "            100)"
                          "         (not (zero? (mod x"
                          "                          3))))"
                          "  (process x)"
                          "  (skip x))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex conditions should format with proper alignment"))))

(deftest test-pair-based-edge-cases
  (testing "when cond has multiple pairs"
    (let [code "(cond a b c d)"
          expected (lines "(cond"
                          "  a"
                          "  b"
                          ""
                          "  c"
                          "  d)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when case has expression"
    (let [code "(case x :a 1 :b 2)"
          expected (lines "(case x"
                          "  :a"
                          "  1"
                          ""
                          "  :b"
                          "  2)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when case has default value"
    (let [code "(case x :a 1 :b 2 \"default\")"
          expected (lines "(case x"
                          "  :a"
                          "  1"
                          ""
                          "  :b"
                          "  2"
                          ""
                          "  \"default\")")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when condp has predicate and expression"
    (let [code "(condp = x :a 1 :b 2)"
          expected (lines "(condp = x"
                          "  :a"
                          "  1"
                          ""
                          "  :b"
                          "  2)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when condp has complex predicate and expression"
    (let [code (lines "(condp (comp odd? inc first) [1 2]"
                      " :a {:foo [1 2]} :b {:bar [3 4]})")
          expected (lines "(condp (comp odd?"
                          "             inc"
                          "             first) [1"
                          "                     2]"
                          "  :a"
                          "  {:foo [1"
                          "         2]}"
                          ""
                          "  :b"
                          "  {:bar [3"
                          "         4]})")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when cond has single pair"
    (let [code "(cond (> x 0) \"positive\")"
          expected (lines "(cond"
                          "  (> x"
                          "     0)"
                          "  \"positive\")")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when pair-based forms are nested"
    (let [code "(cond (< x 0) (case sign :neg -1 :pos 1) (> x 0) \"positive\")"
          expected (lines "(cond"
                          "  (< x"
                          "     0)"
                          "  (case sign"
                          "    :neg"
                          "    -1"
                          ""
                          "    :pos"
                          "    1)"
                          ""
                          "  (> x"
                          "     0)"
                          "  \"positive\")")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when pair-based forms are within other special forms"
    (let [code "(let [x 1] (cond (pos? x) (+ x 1) :else 0))"
          expected (lines "(let [x 1]"
                          "  (cond"
                          "    (pos? x)"
                          "    (+ x"
                          "       1)"
                          ""
                          "    :else"
                          "    0))")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-cond-comment-behavior
  (testing "when cond is preceded by a block comment"
    (let [code "(do
                  ;; This is a comment before cond
                  (cond
                    a b
                    c d))"
          expected (lines "(do"
                          "  ;; This is a comment before cond"
                          "  (cond"
                          "    a"
                          "    b"
                          ""
                          "    c"
                          "    d))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Cond should be properly indented when preceded by a block comment")))
  (testing "when block comments appear between cond pairs"
    (let [code "(cond
                  a b
                  ;; comment between pairs
                  c d
                  ;; another comment
                  e f)"
          expected (lines "(cond"
                          "  a"
                          "  b"
                          ""
                          "  ;; comment between pairs"
                          "  c"
                          "  d"
                          ""
                          "  ;; another comment"
                          "  e"
                          "  f)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Block comments should stick to the next pair, not the previous one"))))

(deftest test-threading-macro-pairs
  (testing "when cond-> has initial expression"
    (let [code "(cond-> x (pos? x) inc (even? x) (* 2))"
          expected (lines "(cond-> x"
                          "  (pos? x)"
                          "  inc"
                          ""
                          "  (even? x)"
                          "  (* 2))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when cond->> has initial expression"
    (let [code "(cond->> x (pos? x) (map inc) (even? (count x)) (filter odd?))"
          expected (lines "(cond->> x"
                          "  (pos? x)"
                          "  (map inc)"
                          ""
                          "  (even? (count x))"
                          "  (filter odd?))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when cond-> has single condition/expression pair"
    (let [code "(cond-> x (pos? x) inc)"
          expected (lines "(cond-> x"
                          "  (pos? x)"
                          "  inc)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when cond->> has nested expressions"
    (let [code "(cond->> data (seq data) (map process) (> (count data) 10) (take 10))"
          expected (lines "(cond->> data"
                          "  (seq data)"
                          "  (map process)"
                          ""
                          "  (> (count data)"
                          "     10)"
                          "  (take 10))")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-try-catch-finally-formatting
  (testing "when try has simple body with catch"
    (let [code (lines "(try (do-something)"
                      " (catch Exception e (handle-error e)))")
          expected (lines "(try"
                          "  (do-something)"
                          "  (catch Exception e"
                          "    (handle-error e)))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when try has multiple expressions with finally"
    (let [code (lines "(try (open-resource)"
                      " (process-data)"
                      " (finally (cleanup)))")
          expected (lines "(try"
                          "  (open-resource)"
                          "  (process-data)"
                          "  (finally"
                          "    (cleanup)))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when try has catch and finally"
    (let [code (lines "(try (risky-operation)"
                      " (catch IOException e (log e))"
                      " (finally (close-resource)))")
          expected (lines "(try"
                          "  (risky-operation)"
                          "  (catch IOException e"
                          "    (log e))"
                          "  (finally"
                          "    (close-resource)))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when try has multiple catch clauses"
    (let [code (lines "(try (operation)"
                      " (catch IOException e1 (handle-io e1))"
                      " (catch SQLException e2 (handle-sql e2)))")
          expected (lines "(try"
                          "  (operation)"
                          "  (catch IOException e1"
                          "    (handle-io e1))"
                          "  (catch SQLException e2"
                          "    (handle-sql e2)))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when try has comments"
    (let [code (lines "(try ;; inline after try"
                      " (operation) ;; inline after operation"
                      " ;; block comment"
                      " (catch IOException e1 (handle-io e1))"
                      "(catch SQLException e2"
                      " ;; comment in catch"
                      " (handle-sql e2)"
                      " ;; trailing comment"
                      "))")
          expected (lines ";; inline after try"
                          "(try"
                          "  (operation) ;; inline after operation"
                          "  ;; block comment"
                          "  (catch IOException e1"
                          "    (handle-io e1))"
                          "  (catch SQLException e2"
                          "    ;; comment in catch"
                          "    (handle-sql e2)"
                          "    ;; trailing comment"
                          "    ))")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-fn-formatting
  (testing "when fn has single arity without name"
    (let [code "(fn [x] (+ x 1))"
          expected (lines "(fn [x]"
                          "  (+ x"
                          "     1))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Anonymous fn should format with args on same line")))
  (testing "when fn has single arity with name"
    (let [code "(fn add-one [x] (+ x 1))"
          expected (lines "(fn add-one [x]"
                          "  (+ x"
                          "     1))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Named fn should keep name and args on same line")))
  (testing "when fn has multiple arities without name"
    (let [code "(fn ([x] (+ x 1)) ([x y] (+ x y)))"
          expected (lines "(fn ([x]"
                          "     (+ x"
                          "        1))"
                          "    ([x"
                          "      y]"
                          "     (+ x"
                          "        y)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-arity fn should have first arity on same line")))
  (testing "when fn has multiple arities with name"
    (let [code "(fn sum ([x] x) ([x y] (+ x y)) ([x y z] (+ x y z)))"
          expected (lines "(fn sum"
                          "  ([x]"
                          "   x)"
                          "  ([x"
                          "    y]"
                          "   (+ x"
                          "      y))"
                          "  ([x"
                          "    y"
                          "    z]"
                          "   (+ x"
                          "      y"
                          "      z)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Named multi-arity fn should format name on first line, arities below")))
  (testing "when fn has destructuring in args"
    (let [code "(fn [{:keys [a b]}] (+ a b))"
          expected (lines "(fn [{:keys [a"
                          "             b]}]"
                          "  (+ a"
                          "     b))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Fn with destructuring should format args properly")))
  (testing "when fn has variadic args"
    (let [code "(fn [x & rest] (apply + x rest))"
          expected (lines "(fn [x"
                          "     & rest]"
                          "  (apply +"
                          "         x"
                          "         rest))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Fn with variadic args should keep & on same line"))))

(deftest test-defmacro-formatting
  (testing "when defmacro has single arity without docstring"
    (let [code "(defmacro unless [test body] `(if (not ~test) ~body))"
          expected (lines "(defmacro unless [test"
                          "                  body]"
                          "  `(if (not ~test)"
                          "     ~body))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Defmacro should format like defn")))
  (testing "when defmacro has docstring"
    (let [code (str "(defmacro unless \"Opposite of when\" [test body] "
                    "`(if (not ~test) ~body))")
          expected (lines "(defmacro unless"
                          "  \"Opposite of when\""
                          "  [test"
                          "   body]"
                          "  `(if (not ~test)"
                          "     ~body))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Defmacro with docstring should format correctly")))
  (testing "when defmacro has multiple arities"
    (let [code (str "(defmacro my-or ([] nil) ([x] x) "
                    "([x & rest] `(let [or# ~x] (if or# or# (my-or ~@rest)))))")
          expected (lines "(defmacro my-or"
                          "  ([]"
                          "   nil)"
                          "  ([x]"
                          "   x)"
                          "  ([x"
                          "    & rest]"
                          "   `(let [or# ~x]"
                          "      (if or#"
                          "        or#"
                          "        (my-or ~@rest)))))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multi-arity defmacro should format each arity on separate lines")))
  (testing "when defmacro has variadic args"
    (let [code "(defmacro debug [& forms] `(do (println \"Debug:\") ~@forms))"
          expected (lines "(defmacro debug [& forms]"
                          "  `(do"
                          "     (println \"Debug:\")"
                          "     ~@forms))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Defmacro with variadic args should format correctly")))
  (testing "when defmacro has destructuring"
    (let [code (str "(defmacro with-config [{:keys [timeout retry]} & body] "
                    "`(binding [*config* {:timeout ~timeout :retry ~retry}] ~@body))")
          expected (lines "(defmacro with-config [{:keys [timeout"
                          "                               retry]}"
                          "                       & body]"
                          "  `(binding [*config* {:timeout ~timeout"
                          "                       :retry ~retry}]"
                          "     ~@body))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Defmacro with destructuring should format correctly"))))

(deftest test-comment-formatting
  (testing "when inline comment follows a form"
    (let [code "(def x 1) ;; the value"
          expected (lines "(def x"
                          "  1) ;; the value")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when standalone comment precedes a form"
    (let [code (lines ";; This defines x"
                      "(def x 1)")
          expected (lines ";; This defines x"
                          "(def x"
                          "  1)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when comment appears between forms"
    (let [code (lines "(def x 1)"
                      ";; Now y"
                      "(def y 2)")
          expected (lines "(def x"
                          "  1)"
                          ""
                          ";; Now y"
                          "(def y"
                          "  2)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when inline comment appears after argument"
    (let [code (lines "(+ 1 ;; first number"
                      "   2)")
          expected (lines "(+ 1 ;; first number"
                          "   2)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when inline comment appears in map"
    (let [code (lines "{:a 1 ;; key a"
                      " :b 2}")
          expected (lines "{:a 1 ;; key a"
                          " :b 2}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when comments have different semicolon counts"
    (let [code (lines "; single"
                      ";; double"
                      ";;; triple"
                      "(def x 1)")
          expected (lines ";; single"
                          ";; double"
                          ";;; triple"
                          "(def x"
                          "  1)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when comment appears with proper indentation"
    (let [code (lines "(defn foo []"
                      "  ;; do something"
                      "  (println))")
          expected (lines "(defn foo []"
                          "  ;; do something"
                          "  (println))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when inline comment follows a closing delimiter"
    (let [code (lines "(defn f []"
                      "  :x) ;; returns x")
          expected (lines "(defn f []"
                          "  :x) ;; returns x")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-comment-edge-cases
  (testing "when comment appears between function name and first argument"
    (let [code (lines "(or ;; comment1"
                      "  x"
                      "  ;; comment2"
                      "  y)")
          ;; comment between fn name and first arg should move before form
          expected (lines ";; comment1"
                          "(or x"
                          "    ;; comment2"
                          "    y)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when standalone comment between function name and first argument"
    (let [code (lines "(or"
                      "  ;; comment1"
                      "  x"
                      "  ;; comment2"
                      "  y)")
          ;; standalone comment between fn name and first arg should move before form
          expected (lines ";; comment1"
                          "(or x"
                          "    ;; comment2"
                          "    y)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when standalone comment is already properly positioned"
    (let [code (lines "(let [x 1]"
                      "  ;; process x"
                      "  (process x))")
          expected (lines "(let [x 1]"
                          "  ;; process x"
                          "  (process x))")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when inline comment has single semicolon"
    (let [code "(def x 1) ; single"
          expected (lines "(def x"
                          "  1) ;; single")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-vector-comments
  (testing "when inline comment appears inside regular vector"
    (let [code (lines "[1 ;; first element"
                      " 2 3]")
          expected (lines "[1 ;; first element"
                          " 2"
                          " 3]")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when block comment appears inside vector"
    (let [code (lines "[1"
                      "  ;; second element"
                      " 2"
                      " 3]")
          expected (lines "[1"
                          " ;; second element"
                          " 2"
                          " 3]")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when multiple inline comments appear in vector"
    (let [code (lines "[1 ;; first"
                      " 2 ;; second"
                      " 3]")
          expected (lines "[1 ;; first"
                          " 2 ;; second"
                          " 3]")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when block comment appears before first element inside vector"
    (let [code (lines "[;; first element"
                      " 1"
                      " 2"
                      " 3]")
          expected (lines "[;; first element"
                          " 1"
                          " 2"
                          " 3]")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when vector has trailing comment"
    (let [code (lines "[1 2 3 ;; trailing inline comment"
                      " ;; trailing block comment"
                      "] ; end of vector")
          expected (lines "[1"
                          " 2"
                          " 3 ;; trailing inline comment"
                          " ;; trailing block comment"
                          " ] ;; end of vector")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-map-comments
  (testing "when inline comment appears after key in map"
    (let [code (lines "{:a ;; first key"
                      " 1 :b 2}")
          expected (lines "{;; first key"
                          " :a 1"
                          " :b 2}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when inline comment appears after value in map"
    (let [code (lines "{:a 1 ;; first value"
                      " :b 2}")
          expected (lines "{:a 1 ;; first value"
                          " :b 2}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when standalone block comment appears inside map"
    (let [code (lines "{:a 1"
                      " ;; second pair"
                      " :b 2}")
          expected (lines "{:a 1"
                          " ;; second pair"
                          " :b 2}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when standalone block comment appears before first key"
    (let [code (lines "{;; first pair"
                      " :a 1"
                      " :b 2}")
          expected (lines "{;; first pair"
                          " :a 1"
                          " :b 2}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when multiple inline comments appear in map"
    (let [code (lines "{:a 1 ;; first"
                      " :b 2 ;; second"
                      " :c 3}")
          expected (lines "{:a 1 ;; first"
                          " :b 2 ;; second"
                          " :c 3}")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when map has trailing comment"
    (let [code "{:a 1 :b 2} ;; end of map"
          expected (lines "{:a 1"
                          " :b 2} ;; end of map")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when inline comment at end of map would consume closing brace"
    (let [code (lines "{:a 1     ; comment"
                      "   }")
          expected (lines "{:a 1 ;; comment"
                          " }")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Inline comment should end the line, closing brace on next line")))
  (testing "when inline comment appears after key"
    (let [code (lines "{;; comment before first key"
                      " :a"
                      "  ; comment after first key"
                      " 1"
                      " ;; comment after value"
                      "   }")
          expected (lines "{;; comment before first key"
                          " ;; comment after first key"
                          " :a 1"
                          " ;; comment after value"
                          " }")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Inline comment should end the line, closing brace on next line")))
  (testing "when nested map has trailing comment"
    (let [code (lines "{:outer {:inner 1 ;; comment"
                      "}}")
          expected (lines "{:outer {:inner 1 ;; comment"
                          "         }}")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested map's closing brace should align with its contents"))))

(deftest test-quoting
  (testing "when using regular quote"
    (let [code "'(foo bar baz)"
          expected (lines "'(foo bar"
                          "      baz)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when using syntax-quote"
    (let [code "`(foo bar baz)"
          expected (lines "`(foo bar"
                          "      baz)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when using unquote in syntax-quote"
    (let [code "`(list ~x ~y)"
          expected (lines "`(list ~x"
                          "       ~y)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when using unquote-splicing"
    (let [code "`(foo ~@args bar)"
          expected (lines "`(foo ~@args"
                          "      bar)")
          formatted (core/format-code code)]
      (is (= expected formatted))))

  (testing "when unquote-splicing has a multi-line collection expression"
    (let [code "`(foo ~@[1 2 3] bar)"
          expected (lines "`(foo ~@[1"
                          "         2"
                          "         3]"
                          "      bar)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when using var quote"
    (let [code "(alter-var-root #'config merge new-config)"
          expected (lines "(alter-var-root #'config"
                          "                merge"
                          "                new-config)")
          formatted (core/format-code code)]
      (is (= expected formatted))))
  (testing "when var quote is in a vector"
    (let [code "[#'foo #'bar #'baz]"
          expected (lines "[#'foo"
                          " #'bar"
                          " #'baz]")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-anonymous-functions
  (testing "when using #(...) forms"
    (let [code "#(foo bar baz)"
          expected (lines "#(foo bar"
                          "      baz)")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-uneval-forms
  (testing "when using #_ reader macro"
    (let [code "#_(foo bar baz)"
          expected (lines "#_(foo bar"
                          "       baz)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Uneval form should format its content"))
    (let [code "(def x #_old-value new-value)"
          expected (lines "(def x"
                          "  #_old-value"
                          "  new-value)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Uneval forms should be positioned correctly in context"))
    (let [code "#_{:a 1 :b 2}"
          expected (lines "#_{:a 1"
                          "   :b 2}")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Uneval maps should format correctly")))
  (testing "when using nested #_ forms"
    (let [code "[#_ #_ x y z]"
          expected (lines "[#_x"
                          " #_y"
                          " z]")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested uneval forms should preserve structure"))
    (let [code "(foo #_ #_ x y z)"
          expected (lines "(foo #_x"
                          "     #_y"
                          "     z)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested uneval in function call should format correctly"))))

(deftest test-set-comments
  (testing "when set has comments in various positions"
    (let [code "#{;; comment before first
               :first
               :middle ;; inline comment
               ;; comment between elements
               :last
               ;; comment after last element
               }"
          expected (lines "#{;; comment before first"
                          "  :first"
                          "  :middle ;; inline comment"
                          "  ;; comment between elements"
                          "  :last"
                          "  ;; comment after last element"
                          "  }")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-ampersand-rest-comments
  "Tests that & rest ;; comment stays together on one line in all contexts"
  (testing "when vector has & rest with inline comment"
    (let [code "[a & rest ;; variadic args
              ]"
          expected (lines "[a"
                          " & rest ;; variadic args"
                          " ]")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "& rest ;; comment should stay together on one line in vector")))
  (testing "when function args have & rest with inline comment"
    (let [code "(defn f [a &
                     rest ;; variadic args
                    ]
              (apply + a rest))"
          expected (lines "(defn f [a"
                          "         & rest ;; variadic args"
                          "         ]"
                          "  (apply +"
                          "         a"
                          "         rest))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "& rest ;; comment should stay together in function arguments")))
  (testing "when function call has & rest with inline comment"
    (let [code "(some-fn a &
                      rest ;; spread args
                     )"
          expected (lines "(some-fn a"
                          "         & rest ;; spread args"
                          "         )")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "& rest ;; comment should stay together in function calls")))
  (testing "when vector has multiple & elements with comments"
    (let [code "[a & rest ;; first comment
              b & more ;; second comment
              ]"
          expected (lines "[a"
                          " & rest ;; first comment"
                          " b"
                          " & more ;; second comment"
                          " ]")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multiple & rest ;; comment pairs should each stay together"))))

(deftest test-do-form-formatting
  (testing "when do has single expression"
    (let [code "(do (println \"hello\"))"
          expected (lines "(do"
                          "  (println \"hello\"))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Single expression in do should be on its own line")))
  (testing "when do has multiple expressions"
    (let [code "(do (println \"starting\") (process-data) (println \"done\"))"
          expected (lines "(do"
                          "  (println \"starting\")"
                          "  (process-data)"
                          "  (println \"done\"))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "All expressions in do should be on separate lines")))
  (testing "when do is nested"
    (let [code "(let [x 1] (do (println x) (inc x)))"
          expected (lines "(let [x 1]"
                          "  (do"
                          "    (println x)"
                          "    (inc x)))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested do should maintain proper indentation")))
  (testing "when do has comments"
    (let [code "(do ;; setup
                  (init)
                  ;; main work
                  (process)
                  (cleanup) ;; inline
                  )"
          expected (lines "(do"
                          "  ;; setup"
                          "  (init)"
                          "  ;; main work"
                          "  (process)"
                          "  (cleanup) ;; inline"
                          "  )")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Comments in do should be preserved")))
  (testing "when do has complex nested expressions"
    (let [code "(do (let [x 1] x) (if true :yes :no) {:a 1 :b 2})"
          expected (lines "(do"
                          "  (let [x 1]"
                          "    x)"
                          "  (if true"
                          "    :yes"
                          "    :no)"
                          "  {:a 1"
                          "   :b 2})")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex expressions in do should each format independently")))
  (testing "when do has only inline comment and no body"
    (let [code "(do ;; just a comment\n)"
          expected (lines "(do ;; just a comment"
                          "  )")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Inline comment should stay on same line as do")))
  (testing "when do form has block and inline comments between inner nodes"
    (let [code "(do
                  (def x 1)
                  ;; block comment between nodes
                  (def y 2) ;; inline after node
                  ;; another block
                  (+ x y) ;; final inline
                  )"
          expected (lines "(do"
                          "  (def x"
                          "    1)"
                          "  ;; block comment between nodes"
                          "  (def y"
                          "    2) ;; inline after node"
                          "  ;; another block"
                          "  (+ x"
                          "     y) ;; final inline"
                          "  )")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Block and inline comments between nodes should be preserved"))))

(deftest test-comment-form-formatting
  (testing "when comment has single expression"
    (let [code "(comment (println \"debug\"))"
          expected (lines "(comment"
                          "  (println \"debug\"))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Single expression in comment should be on its own line")))
  (testing "when comment has multiple expressions"
    (let [code "(comment (def x 1) (def y 2) (+ x y))"
          expected (lines "(comment"
                          "  (def x"
                          "    1)"
                          "  (def y"
                          "    2)"
                          "  (+ x"
                          "     y))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "All expressions in comment should be on separate lines and formatted")))
  (testing "when comment form is nested"
    (let [code "(defn foo [] (comment (println \"TODO: implement\") nil) :placeholder)"
          expected (lines "(defn foo []"
                          "  (comment"
                          "    (println \"TODO: implement\")"
                          "    nil)"
                          "  :placeholder)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Nested comment form should maintain proper indentation")))
  (testing "when comment form has inline comments"
    (let [code "(comment
                  (def x 1) ;; temporary
                  ;; more debugging
                  (println x))"
          expected (lines "(comment"
                          "  (def x"
                          "    1) ;; temporary"
                          "  ;; more debugging"
                          "  (println x))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Comments inside comment form should be preserved")))
  (testing "when comment form has text and code"
    (let [code "(comment
                  \"Some documentation\"
                  (example-usage)
                  \"Another note\")"
          expected (lines "(comment"
                          "  \"Some documentation\""
                          "  (example-usage)"
                          "  \"Another note\")")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Text and code in comment form should format correctly")))
  (testing "when comment has only inline comment and no body"
    (let [code "(comment ;; just a comment\n)"
          expected (lines "(comment ;; just a comment"
                          "  )")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Closing paren should not be commented out")))
  (testing "when comment form has block and inline comments between inner nodes"
    (let [code "(comment
                  (def x 1)
                  ;; block comment between nodes
                  (def y 2) ;; inline after node
                  ;; another block
                  (+ x y) ;; final inline
                  )"
          expected (lines "(comment"
                          "  (def x"
                          "    1)"
                          "  ;; block comment between nodes"
                          "  (def y"
                          "    2) ;; inline after node"
                          "  ;; another block"
                          "  (+ x"
                          "     y) ;; final inline"
                          "  )")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Block and inline comments between nodes should be preserved"))))

(deftest test-reduce-with-cond-indentation
  (testing "when reduce has fn with destructuring and cond in body"
    (let [code (lines "(reduce (fn [{:keys [result pending]} elem]"
                      "(cond"
                      ";; Current element matches predicate"
                      "(pred elem)"
                      "{:result result"
                      ":pending elem}"
                      ""
                      ";; We have a pending element"
                      "(some? pending)"
                      "{:result (conj result [pending elem])"
                      ":pending nil}"
                      ""
                      ";; Normal element"
                      ":else"
                      "{:result (conj result elem)"
                      ":pending nil}))"
                      "{:result []"
                      ":pending nil}"
                      "elements)")
          ;; This is what we WANT (desired behavior)
          expected (lines "(reduce (fn [{:keys [result"
                          "                     pending]}"
                          "             elem]"
                          "          (cond"
                          "            ;; Current element matches predicate"
                          "            (pred elem)"
                          "            {:result result"
                          "             :pending elem}"
                          ""
                          "            ;; We have a pending element"
                          "            (some? pending)"
                          "            {:result (conj result"
                          "                           [pending"
                          "                            elem])"
                          "             :pending nil}"
                          ""
                          "            ;; Normal element"
                          "            :else"
                          "            {:result (conj result"
                          "                           elem)"
                          "             :pending nil}))"
                          "        {:result []"
                          "         :pending nil}"
                          "        elements)")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "cond should be properly indented with comments in correct position"))))
