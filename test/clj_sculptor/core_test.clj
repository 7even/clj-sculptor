(ns clj-sculptor.core-test
  (:require [clojure.test :refer :all]
            [clj-sculptor.core :as core]
            [rewrite-clj.zip :as z]))

(deftest parse-string-test
  (testing "Parse simple form"
    (let [code "(def x 1)"
          parsed (core/parse-string code)]
      (is (some? parsed))
      (is (= "(def x 1)" (z/string parsed)))))
  (testing "Parse form with comments"
    (let [code ";; This is a comment\n(def x 1)"
          parsed (core/parse-string code)]
      (is (some? parsed))
      (is (= code (z/root-string parsed)))))
  (testing "Parse multiple forms"
    (let [code "(def x 1)\n(def y 2)"
          parsed (core/parse-string code)]
      (is (some? parsed))
      (is (= code (z/root-string parsed))))))

(deftest format-code-test
  (testing "Format simple code"
    (let [code "(def x 1)"
          parsed (core/parse-string code)
          formatted (core/format-code parsed)]
      (is (= code formatted))))
  (testing "Format code preserves comments"
    (let [code ";; Comment\n(def x 1)"
          parsed (core/parse-string code)
          formatted (core/format-code parsed)]
      (is (= code formatted)))))

(deftest apply-formatting-rules-test
  (testing "Apply formatting rules returns zipper"
    (let [code "(def x 1)"
          parsed (core/parse-string code)
          result (core/apply-formatting-rules parsed)]
      (is (some? result))
      (is (= code (z/root-string result))))))
