(ns bobisageek.aoc2019.intcode-tests
  (:require [clojure.test :refer :all]
            [bobisageek.aoc2019.intcode :as i]
            [clojure.string :as str])
  (:import (java.io StringWriter)))

(defn test-run
  "Run the intcode interpreter and returns a map of :final-tape and :output-lines.
  There is no interactive input; any input can be passed as a string (space delimited
  for multiple 'lines' of input)"
  ([initial-tape] (test-run initial-tape ""))
  ([initial-tape inputs]
   (let [out-str (StringWriter.)]
     (binding [*out* out-str]
       (let [final-tape (with-in-str inputs (i/intcode-run initial-tape))]
         {:final-tape final-tape :output-lines (str/split-lines (str out-str))})))))


(deftest opcodes-and-math
  (testing "immediate mode uses value instead of positions"
    (is (=
          (i/intcode-run [1002 4 3 4 33])
          [1002 4 3 4 99]))))

(deftest IO-ops
  (testing "input can write to tape using positional mode"
    (are [input-str init-tape output-tape]
      (= (-> (test-run init-tape input-str) :final-tape) output-tape)
      "12345" [3 0 99] [12345 0 99]
      "\"a\"" [3 0 99] ["a" 0 99]))
  (testing "output op can write in position mode"
    (are [init-tape output-lines]
      (= (-> (test-run init-tape) :output-lines) output-lines)
      [4 0 4 4 99] ["4" "99"]))
  (testing "output op can write in immediate mode"
    (are [init-tape output-lines]
      (= (-> (test-run init-tape) :output-lines) output-lines)
      [104 0 104 50 99] ["0" "50"]))
  (testing "mixed input and output tests"
    (are [init-tape inputs output-lines]
      (= (-> (test-run init-tape inputs) :output-lines) output-lines)
      [104 12 3 0 4 0 99] "5" ["12" "5"])))

