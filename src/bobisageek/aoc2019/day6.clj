(ns bobisageek.aoc2019.day6
  (:require [bobisageek.aoc2019.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

#_(u/day-lines "6min")
(let [a #"\)"]
  (str/split "ab)c" a))
(defn parse [line]
  (let [paren-index (str/index-of line ")")
        halves      [(take paren-index line) (drop (inc paren-index) line)]
        [orbiter orbitee] (map (partial apply str) halves)]
    [orbitee orbiter]))
(defn parse-all [lines]
  (into {} (map parse lines)))

(defn part1 [m]
  (letfn [(count-hops [k]
            (loop [cur k
                   count 0]
              (if (= cur "COM")
                count
                (recur (m cur) (inc count)))))]
    (reduce + (map count-hops (keys m)))))

#_(part1 (parse-all (u/day-lines "6")))

(defn path-to [m source endpoint]
  (loop [path {}
         cur (m source)
         counter 0]
    (if (= cur endpoint)
      path
      (recur (assoc path cur counter) (m cur) (inc counter)))))

#_ (path-to (parse-all (u/day-lines "6-2min")) "SAN" "COM")

(defn part2 [m source target]
  (let [source-path (set/map-invert (path-to m source "COM"))
        target-path (path-to m target "COM")]
    (loop [index 0]
      (let [source-point (source-path index)
            target-index (target-path source-point)]
        (cond
          (nil? source-point) "no intersection found on graph"
          (not (nil? target-index)) (+ index target-index)
          :else (recur (inc index)))))))

#_ (part2 (parse-all (u/day-lines "6")) "YOU" "SAN")
