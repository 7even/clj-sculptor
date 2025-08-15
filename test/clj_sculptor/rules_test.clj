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
          expected (lines "(def x 1)"
                          ""
                          "(def y 2)")
          formatted (core/format-code code)]
      (is (= expected formatted)))))

(deftest test-leading-whitespace-rule
  (testing "when document starts with whitespace"
    (let [code "  (def x 1)"
          expected "(def x 1)"
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
      (is (= expected formatted) "First arg should move to same line"))))

(deftest test-multiple-top-level-forms
  (testing "when file contains multiple varied top-level forms"
    (let [code (str "(defn helper [x] (+ x 1)) "
                    "(def config {:key \"value\"}) "
                    "(defmethod multi-fn :type [{:keys [data]}] (process data))")
          expected (lines "(defn helper [x]"
                          "  (+ x"
                          "     1))"
                          ""
                          "(def config {:key \"value\"})"
                          ""
                          "(defmethod multi-fn :type [{:keys [data]}]"
                          "  (process data))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Multiple top-level forms should be properly separated and formatted")))

  (testing "when multiple forms have inconsistent spacing between them"
    (let [code "(def x 1)   (def y 2)    (defn add [a b] (+ a b))"
          expected (lines "(def x 1)"
                          ""
                          "(def y 2)"
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
          expected (lines "(def data [{:users [{:name \"Alice\""
                          "                     :roles #{:admin"
                          "                              :user}}]}"
                          "           {:system {:config {:db {:host \"localhost\"}}}}])"
                          ""
                          "(defn simple [input]"
                          "  (process input))")
          formatted (core/format-code code)]
      (is (= expected formatted)
          "Complex nested structures in multiple forms should maintain proper alignment"))))
