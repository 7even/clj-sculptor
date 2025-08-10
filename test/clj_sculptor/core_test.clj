(ns clj-sculptor.core-test
  (:require [clj-sculptor.core :as core]
            [clojure.test :refer :all]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]))

(deftest parse-string-test
  (testing "when given simple form"
    (let [code "(def x 1)"
          parsed (core/parse-string code)]
      (is (some? parsed))
      (is (= "(def x 1)" (z/string parsed)))))
  (testing "when given form with comments"
    (let [code ";; This is a comment\n(def x 1)"
          parsed (core/parse-string code)]
      (is (some? parsed))
      (is (= code (z/root-string parsed)))))
  (testing "when given multiple forms"
    (let [code "(def x 1)\n(def y 2)"
          parsed (core/parse-string code)]
      (is (some? parsed))
      (is (= code (z/root-string parsed))))))

(deftest multimethod-dispatch-test
  (testing "when dispatching on different node types"
    ;; Test that the multimethod exists and has default implementation
    (let [zloc (z/of-string "(def x 1)")]
      (is (= zloc (core/apply-rules zloc))))

    ;; Test with whitespace node
    (let [zloc (z/of-string "   ")]
      (is (some? (core/apply-rules zloc))))))

(deftest whitespace-helper-test
  (testing "when checking node types"
    (is (true? (core/whitespace-node? (n/whitespace-node " "))))
    (is (false? (core/whitespace-node? (n/token-node :foo))))

    (is (true? (core/linebreak-node? (n/newline-node "\n"))))
    (is (false? (core/linebreak-node? (n/whitespace-node " "))))

    (is (true? (core/comma-node? (n/comma-node ","))))
    (is (false? (core/comma-node? (n/whitespace-node " ")))))

  (testing "when counting newlines"
    (is (= 2 (core/count-newlines (n/newlines 2))))
    (is (= 0 (core/count-newlines (n/spaces 2)))))

  (testing "when creating whitespace nodes"
    (is (= "  " (str (core/create-spaces 2))))
    (is (= "\n\n" (str (core/create-newlines 2))))))

(deftest remove-trailing-whitespace-test
  (testing "when removing trailing spaces"
    (is (= "hello\nworld\n" (core/remove-trailing-whitespace "hello  \nworld  \n")))
    (is (= "hello" (core/remove-trailing-whitespace "hello  ")))
    (is (= "hello\n" (core/remove-trailing-whitespace "hello\n")))))

(deftest format-code-test
  (testing "when formatting simple code"
    (let [code "(def x 1)"
          formatted (core/format-code code)]
      (is (= code formatted))))

  (testing "when formatting code with comments"
    (let [code ";; Comment\n(def x 1)"
          formatted (core/format-code code)]
      (is (string? formatted))))

  (testing "when formatting multiple top-level forms"
    (let [code "(def x 1)\n\n(def y 2)"
          formatted (core/format-code code)]
      (is (= code formatted))))

  (testing "when formatting multiple forms with trailing whitespace"
    (let [code "(def x 1)  \n\n(def y 2)  "
          expected "(def x 1)\n\n(def y 2)"
          formatted (core/format-code code)]
      (is (= expected formatted)))))
