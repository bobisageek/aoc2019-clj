(ns bobisageek.aoc2019.day4
  (:require [clojure.string :as str]))

(defn repeats-and-increases? [num]
  (let [s (map int (str num))]
    (and (not= (dedupe s) s) (apply <= s))))

; region part 1
#_(count (filter repeats-and-increases? (range 265275 781585)))
; endregion part 1

(defn increases-and-has-exactly-2? [num]
  (let [s (map int (str num))]
    (and (apply <= s) (some #{2} (map count (partition-by identity s))))))

; region part 2
#_(count (filter increases-and-has-exactly-2? (range 265275 781585)))
; endregion part 2
