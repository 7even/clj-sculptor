(ns clj-sculptor.cli-test
  (:require [clojure.test :refer :all]
            [clj-sculptor.cli :as cli]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File StringWriter]))

(deftest cli-help-test
  (testing "CLI shows help"
    (let [output (with-out-str (cli/-main "--help"))]
      (is (str/includes? output "clj-sculptor"))
      (is (str/includes? output "Show help")))))

(deftest cli-main-test
  (testing "CLI processes input file"
    (let [temp-file (File/createTempFile "test" ".clj")
          test-code "(def x 1)"]
      (try
        (spit temp-file test-code)
        (let [output (with-out-str (cli/-main "-i" (.getPath temp-file)))]
          (is (= (str test-code "\n") output)))
        (finally
          (.delete temp-file)))))
  (testing "CLI writes to output file"
    (let [input-file (File/createTempFile "input" ".clj")
          output-file (File/createTempFile "output" ".clj")
          test-code "(def x 1)"]
      (try
        (spit input-file test-code)
        (cli/-main "-i" (.getPath input-file) "-o" (.getPath output-file))
        (is (= test-code (slurp output-file)))
        (finally
          (.delete input-file)
          (.delete output-file))))))

(deftest main-exec-fn-test
  (testing "main function works with exec-fn format"
    (let [temp-file (File/createTempFile "test" ".clj")
          test-code "(def x 1)"]
      (try
        (spit temp-file test-code)
        (let [output (with-out-str (cli/main {:input (.getPath temp-file)}))]
          (is (= (str test-code "\n") output)))
        (finally
          (.delete temp-file))))))
