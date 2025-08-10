(ns clj-sculptor.core-test
  (:require [clojure.test :refer :all]
            [clj-sculptor.core :as core]
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

(deftest format-code-test
  (testing "when formatting simple code"
    (let [code "(def x 1)"
          parsed (core/parse-string code)
          formatted (core/format-code parsed)]
      (is (= code formatted))))
  (testing "when formatting code with comments"
    (let [code ";; Comment\n(def x 1)"
          parsed (core/parse-string code)
          formatted (core/format-code parsed)]
      (is (= code formatted)))))

(deftest apply-formatting-rules-test
  (testing "when applying formatting rules"
    (let [code "(def x 1)"
          parsed (core/parse-string code)
          result (core/apply-formatting-rules parsed)]
      (is (some? result))
      (is (= code (z/root-string result))))))
