(ns clj-sculptor.cli-test
  (:require [clj-sculptor.cli :as cli]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all])
  (:import (java.io File
                    StringWriter)))

(deftest cli-help-test
  (testing "when given --help flag"
    (let [output (with-out-str (cli/-main "--help"))]
      (is (str/includes? output
                         "clj-sculptor"))
      (is (str/includes? output
                         "Show help")))))

(deftest cli-main-test
  (testing "when given input file"
    (let [temp-file (File/createTempFile "test"
                                         ".clj")
          test-code "(def x 1)"]
      (try
        (spit temp-file
              test-code)
        (let [output (with-out-str (cli/-main "-i"
                                              (.getPath temp-file)))]
          (is (= "(def x\n  1)\n"
                 output)))
        (finally
          (.delete temp-file)))))
  (testing "when given input and output files"
    (let [input-file (File/createTempFile "input"
                                          ".clj")
          output-file (File/createTempFile "output"
                                           ".clj")
          test-code "(def x 1)"]
      (try
        (spit input-file
              test-code)
        (cli/-main "-i"
                   (.getPath input-file)
                   "-o"
                   (.getPath output-file))
        (is (= "(def x\n  1)\n"
               (slurp output-file)))
        (finally
          (.delete input-file)
          (.delete output-file))))))

(deftest main-exec-fn-test
  (testing "when called via exec-fn with options map"
    (let [temp-file (File/createTempFile "test"
                                         ".clj")
          test-code "(def x 1)"]
      (try
        (spit temp-file
              test-code)
        (let [output (with-out-str (cli/main {:input (.getPath temp-file)}))]
          (is (= "(def x\n  1)\n"
                 output)))
        (finally
          (.delete temp-file))))))
