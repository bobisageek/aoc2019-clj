(ns bobisageek.aoc2019.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- slurp-res [resource-path]
  (io! (slurp (io/resource resource-path))))

(defn day-text [day]
  (let [res-path (str "aoc2019/" day ".txt")]
    (slurp-res res-path)))

(defn day-lines [day]
  (str/split-lines (day-text day)))

(defn rgx-extract-pieces [re lines]
  (map (comp (partial drop 1) first (partial re-seq re)) lines))

(def count-if (comp count filter))
