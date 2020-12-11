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

(deftest jump-if-non-zero
  (testing "position mode for test and consequent"
    (are [init-tape output]
      (= (-> (test-run init-tape) :output-lines) output)
      ; if the zeroth value is not zero jump to the position described by the zeroth value
      ; should skip to index 5 without printing
      [5 0 0 104 2 99] [""]
      ; if the second value is not zero jump to the position described by the zeroth value
      ; should not skip so it should print the 2 and then halt
      [5 2 0 104 2 99] ["2"]))
  (testing "immediate mode for test and consequent"
    (are [init-tape output]
      (= (-> (test-run init-tape) :output-lines) output)
      ; if the test value is not zero jump to the position in the consequent value
      [1105 1 5 104 2 99] [""]
      [1105 0 5 104 2 99] ["2"]))
  (testing "mix immediate and position mode"
    (are [init-tape output]
      (= (-> (test-run init-tape) :output-lines) output)
      ; immediate mode for test parameter only
      ; true
      [105 1 6 104 2 99 5] [""]
      ; false
      [105 0 6 104 2 99 5] ["2"]
      ; immediate more for consequent
      ; true
      [1005 0 5 104 2 99] [""]
      ; false
      [1005 6 5 104 2 99 0] ["2"]))
  (testing "cases from site"
    (are [init-tape input output]
      (= (-> (test-run init-tape input) :output-lines) output)
      ; test if input is equal to zero (position mode)
      [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] "8" ["1"]
      [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] "0" ["0"]
      ; test if input is equal to zero (immeditate mode)
      [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] "0" ["0"]
      [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] "74" ["1"])))

(deftest binary-compares
  (testing "cases from site"
    (are [init-tape input output]
      (= (-> (test-run init-tape input) :output-lines) output)
      ; test if input = 8 (position mode)
      [3 9 8 9 10 9 4 9 99 -1 8] "8" ["1"]
      [3 9 8 9 10 9 4 9 99 -1 8] "7" ["0"]
      ; test if input < 8 (position mode)
      [3 9 7 9 10 9 4 9 99 -1 8] "3" ["1"]
      [3 9 7 9 10 9 4 9 99 -1 8] "8" ["0"]
      [3 9 7 9 10 9 4 9 99 -1 8] "100" ["0"]
      ; test = 8 (immediate mode)
      [3 3 1108 -1 8 3 4 3 99] "8" ["1"]
      [3 3 1108 -1 8 3 4 3 99] "17" ["0"]
      ; test < 8 (immediate mode)
      [3 3 1107 -1 8 3 4 3 99] "17" ["0"]
      [3 3 1107 -1 8 3 4 3 99] "8" ["0"]
      [3 3 1107 -1 8 3 4 3 99] "1" ["1"]
      ; bigger test - output 999 for < 8, 1000 for = 8, 1001 for > 8
      [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 
       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 
       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] "-13" ["999"]
      [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] "8" ["1000"]
      [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
       1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
       999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99] "999" ["1001"])))



