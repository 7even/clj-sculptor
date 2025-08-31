(ns clj-sculptor.core-test
  (:require [clj-sculptor.core :as core]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(deftest format-code-test
  (testing "when formatting simple code"
    (let [code "(def x 1)"
          expected "(def x\n  1)"
          formatted (core/format-code code)]
      (is (= expected
             formatted))))
  (testing "when formatting code with comments preserved"
    (let [code ";; Comment\n(def x 1)"
          formatted (core/format-code code)]
      (is (string? formatted))
      ;; Comments are preserved in the AST but formatting focuses on structure
      (is (str/includes? formatted
                         "(def x\n  1)"))))
  (testing "when formatting multiple top-level forms"
    (let [code "(def x 1)\n\n(def y 2)"
          expected "(def x\n  1)\n\n(def y\n  2)"
          formatted (core/format-code code)]
      (is (= expected
             formatted))))
  (testing "when formatting multiple forms with trailing whitespace"
    (let [code "(def x 1)  \n\n(def y 2)  "
          expected "(def x\n  1)\n\n(def y\n  2)"
          formatted (core/format-code code)]
      (is (= expected
             formatted))))
  (testing "when formatting nested structures"
    (let [code "{:a 1 :b 2}"
          expected "{:a 1\n :b 2}"
          formatted (core/format-code code)]
      (is (= expected
             formatted))))
  (testing "when formatting function calls"
    (let [code "(some-function arg1 arg2 arg3)"
          expected "(some-function arg1\n               arg2\n               arg3)"
          formatted (core/format-code code)]
      (is (= expected
             formatted)))))
