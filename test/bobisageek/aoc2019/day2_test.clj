(ns bobisageek.aoc2019.day2-test
  (:require [clojure.test :refer :all]
            [bobisageek.aoc2019.day2 :refer :all]))

(deftest intcode-day2
  (testing "part1 returns 6327510"
    (is (= 6327510 (part1))))
  (testing "part2 returns 4112"
    (is (= 4112 (part2)))))

