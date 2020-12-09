(ns bobisageek.aoc2019.day2
  (:require [bobisageek.aoc2019.utils :as u]
            [bobisageek.aoc2019.intcode :as i]
            [clojure.string :as str]))

#_(->> (u/day-lines "2min")
    (map (comp vec i/text->intcode-tape))
    (map i/intcode-run))

; region part 1
(defn part1 []
  (-> (u/day-text "2")
    (i/text->intcode-tape)
    vec
    (assoc 1 12 2 2)
    (i/intcode-run)
    (get 0)))

; endregion

; region part 2

(defn run-with [tape noun verb]
  (i/intcode-run (assoc tape 1 noun 2 verb)))

(defn part2 []
  (let [tape           (vec (i/text->intcode-tape (u/day-text 2)))
        candidate-nums (range 100)
        [_ n v] ((comp first filter)
                 #(= 19690720 (first %))
                 (for [n candidate-nums
                       v candidate-nums]
                   [(first (run-with tape n v)) n v]))]
    (+ v (* 100 n))))
