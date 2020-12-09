(ns bobisageek.aoc2019.day1
  (:require [bobisageek.aoc2019.utils :as u]))

(def parse-inputs (partial map #(Integer/parseInt %)))

#_(parse-inputs (u/day-lines 1))

(def fuel-for #(-> % (/ 3) int (- 2)))

(defn part2-fuel-for [mass]
  (drop 1 (take-while pos? (iterate fuel-for mass))))

(defn sum-by [f coll]
  (apply + 0N (map f coll)))

#_(let [masses (parse-inputs (u/day-lines 1))]
    (println (sum-by fuel-for masses))
    (println (sum-by #(apply + (part2-fuel-for %)) masses)))
