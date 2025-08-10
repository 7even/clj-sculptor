(ns clj-sculptor.rules-test
  (:require [clojure.test :refer :all]
            [clj-sculptor.core :as core]))

(deftest test-trailing-whitespace-rule
  (testing "when removing trailing whitespace"
    (let [code-with-trailing "(defn foo []  \n  :bar)  "
          expected "(defn foo []\n  :bar)"
          formatted (core/format-code code-with-trailing)]
      (is (= expected formatted))))

  (testing "when code has no trailing whitespace"
    (let [clean-code "(defn foo []\n  :bar)"
          formatted (core/format-code clean-code)]
      (is (= clean-code formatted))))

  (testing "when whitespace has multiple trailing spaces"
    (let [code "  (def x 1)   \n  (def y 2)  "
          expected "  (def x 1)\n  (def y 2)"
          formatted (core/format-code code)]
      (is (= expected formatted)))))
