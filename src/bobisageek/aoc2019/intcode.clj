(ns bobisageek.aoc2019.intcode
  (:require [clojure.string :as str]))

(defn text->intcode-tape
  "Builds an intcode tape from a block of numbers.
  All numbers must be comma-separated, even if there is whitespace/newlines between them."
  [input]
  (as-> input $
    (str/escape $ {\return  ""
                   \newline ""
                   \space   ""})
    (str/split $ #",")
    (map #(Integer/parseInt %) $)
    (vec $)))

; region parameter functions

(defn decide-param-mode
  "Takes a parser determinant (:position for always positional,
  :op for immediate or positional (based on opcode).
  Returns :position or :immediate."
  [parser-determinant opcode-determinant]
  (if (= :write-pos parser-determinant)
    :immediate
    ({\0 :position \1 :immediate} opcode-determinant)))

(defn parameter-modes
  ([opcode param-count] (parameter-modes opcode param-count (repeat param-count :op)))
  ([opcode param-count parser-determinants]
   (let [givens (drop 2 (reverse (str opcode)))]
     (->> (concat givens (repeat \0))
       (take param-count)
       (map decide-param-mode parser-determinants)
       vec))))

(defn find-value [tape pos mode]
  (case mode
    :immediate pos
    :position (tape pos)))

(defn param-parse
  "takes a tape, a starting pos, and a mode spec, which is a vector of designations of:
  :op - treat this as a parameter (can be either immediate or positional mode)
  :write-pos - a write position"
  [tape pos mode-spec]
  (let [param-count (count mode-spec)
        [opcode & params-from-tape] (take (inc param-count) (subvec tape pos))
        modes       (parameter-modes opcode param-count mode-spec)]
    (into [opcode] (map (partial find-value tape) params-from-tape modes))))
; endregion parameter functions

; region logic functions

(defn binary-arithmetic-and-replace [tape pos func]
  (let [[_ f s replace-pos] (param-parse tape pos [:op :op :write-pos])]
    [(assoc tape replace-pos (func f s)) (+ pos 4)]))

(defn jump-if [tape pos pred]
  (let [[_ test-val consequent-val] (param-parse tape pos [:op :op])]
    (if (pred test-val)
      [tape consequent-val]
      [tape (+ pos 3)])))

(defn compare-and-write [tape pos compare-func]
  (let [[_ lhs rhs replace-pos] (param-parse tape pos [:op :op :write-pos])
        output (if (compare-func lhs rhs) 1 0)]
    [(assoc tape replace-pos output) (+ pos 4)]))

; endregion logic functions

(defmulti intcode-op
  "Takes a tape and a current position, returns a possibly modified tape
   and a new position (or nil if the interpretation should halt)"
  {:arglists '([tape pos])}
  (fn [tape pos] (mod (tape pos) 100)))


; unknown opcode (basically, just explode)
(defmethod intcode-op :default [tape pos]
  (let [bad-opcode (tape pos)]
    (throw (ex-info "Invalid opcode - program aborted"
             {:cause       "Bad Opcode"
              :opcode      bad-opcode
              :at-position pos}))))
; halt
(defmethod intcode-op 99 [tape _] [tape nil])

; addition
(defmethod intcode-op 1 [tape pos]
  (binary-arithmetic-and-replace tape pos +))

; multiplication
(defmethod intcode-op 2 [tape pos]
  (binary-arithmetic-and-replace tape pos *))

; input (one number)
(defmethod intcode-op 3 [tape pos]
  (let [[_ replace-pos] (subvec tape pos)
        input-value (read)]
    [(assoc tape replace-pos input-value) (+ pos 2)]))

; output (one number)
(defmethod intcode-op 4 [tape pos]
  (let [[_ output-value] (param-parse tape pos [:op])]
    (println output-value)
    [tape (+ pos 2)]))

; jump if non-zero
(defmethod intcode-op 5 [tape pos]
  (jump-if tape pos (complement zero?)))
; jump if zero
(defmethod intcode-op 6 [tape pos]
  (jump-if tape pos zero?))

; test less than
(defmethod intcode-op 7 [tape pos]
  (compare-and-write tape pos <))
; test equal
(defmethod intcode-op 8 [tape pos]
  (compare-and-write tape pos =))

(defn intcode-run [init-tape]
  (loop [[tape pos] [init-tape 0]]
    (if (nil? pos)
      tape
      (recur (intcode-op tape pos)))))
