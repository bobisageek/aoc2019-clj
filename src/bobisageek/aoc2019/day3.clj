(ns bobisageek.aoc2019.day3
  (:require [bobisageek.aoc2019.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

#_(partition 2 (u/day-lines "3min"))

(defn parse-line [l]
  (as-> l $
    (str/split $ #",")
    (mapv #(let [[dir amount] (drop 1 (re-matches #"([RULD])(\d+)" %))]
             [dir (Integer/parseInt amount)]) $)))

; region part 1
(defn mrange [start f delta]
  (take delta (drop 1 (iterate f start))))

(defn path-r [[s x y] [dir amount]]
  (let [mr         (fn [xf yf] (map vector (mrange x xf amount) (mrange y yf amount)))
        new-points (apply mr (case dir
                               "R" (vector inc identity)
                               "L" (vector dec identity)
                               "U" (vector identity inc)
                               "D" (vector identity dec)))
        [new-x new-y] (last new-points)]
    [(into s new-points) new-x new-y]))

(defn points-on [path]
  (first (reduce path-r [#{} 0 0] path)))

(defn manhattan-distance [start end]
  (apply + (map (comp #(Math/abs ^long %) -) start end)))


(defn part1 [path1 path2]
  (->> [path1 path2]
    (map points-on)
    (apply set/intersection)
    (map (partial manhattan-distance [0 0]))
    (apply min)))

#_(->> (map parse-line (u/day-lines "3min"))
    (partition 2)
    (map (partial apply part1)))

#_(let [p (map parse-line (u/day-lines "3"))]
    (apply part1 p))

; endregion part 1

(defn dir-seq [v]
  (mapcat (fn [[c t]] (repeat t c)) v))

#_(map (comp dir-seq parse-line) (take 2 (u/day-lines "3min")))
(defn part2-seq [path]
  (let [m {"R" [1 0] "L" [-1 0] "U" [0 1] "D" [0 -1]}
        start-point [0 0]
        delta-and-num (map #(vector (m %1) %2) path (iterate inc 1))]
    (reduce (fn [[m point] [delta index]]
              (let [new-point (mapv + point delta)]
                [(update m new-point (fnil identity index)) new-point])) [{} start-point] delta-and-num)))

#_ (part2-seq (dir-seq (parse-line (first (u/day-lines "3min")))))


(defn part2 [paths]
  (->> paths
    (map (comp first part2-seq dir-seq))))

#_(let [points (->> (u/day-lines "3")
                 (map parse-line)
                 part2)
        intersections (apply set/intersection (map (comp set keys) points))]
    (->> (map (fn [p] (apply + (map #(get % p) points))) intersections)
      (apply min)))


