(ns bobisageek.aoc2019.intcode
  (:require [clojure.string :as str]))

(def int-div (comp int /))

(defn parameter-modes [opcode param-count]
  (let [givens (drop 2 (reverse (str opcode)))]
    (->> (concat givens (repeat \0))
      (take param-count)
      (map {\0 :position \1 :immediate})
      vec)))

(defn find-value [tape pos mode]
  (case mode
    :immediate pos
    :position (tape pos)))

(defn text->intcode-tape [input]
  (as-> input $
    (str/escape $ {\return  ""
                   \newline ""
                   \space   ""})
    (str/split $ #",")
    (map #(Integer/parseInt %) $)
    (vec $)))

(defmulti intcode-op
  "Takes a tape and a current position, returns a possibly modified tape
   and a new position (or nil if the interpretation should halt)"
  {:arglists '([tape pos])}
  (fn [tape pos] (mod (tape pos) 100)))

(defn binary-arithmetic-and-replace [tape pos func]
  (let [[opcode f s replace-pos] (subvec tape pos)
        [f-mode s-mode] (parameter-modes opcode 2)]
    [(assoc tape replace-pos (func (find-value tape f f-mode) (find-value tape s s-mode))) (+ pos 4)]))

(defmethod intcode-op 99 [tape _] [tape nil])
(defmethod intcode-op 1 [tape pos]
  (binary-arithmetic-and-replace tape pos +))
(defmethod intcode-op 2 [tape pos]
  (binary-arithmetic-and-replace tape pos *))
(defmethod intcode-op 3 [tape pos]
  (let [[_ replace-pos] (subvec tape pos)
        input-value (read)]
    [(assoc tape replace-pos input-value) (+ pos 2)]))
(defmethod intcode-op 4 [tape pos]
  (let [[opcode output-from-pos] (subvec tape pos)
        [pos-mode] (parameter-modes opcode 1)
        output-value (find-value tape output-from-pos pos-mode)]
    (println output-value)
    [tape (+ pos 2)]))

(defn intcode-run [init-tape]
  (loop [[tape pos] [init-tape 0]]
    (if (nil? pos)
      tape
      (recur (intcode-op tape pos)))))
