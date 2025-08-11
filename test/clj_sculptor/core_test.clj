(ns clj-sculptor.core-test
  (:require [clj-sculptor.core :as core]
            [clojure.test :refer :all]
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
