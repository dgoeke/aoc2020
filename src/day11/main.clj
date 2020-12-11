(ns aoc.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def lines (->> "day11-input.txt"
                io/resource slurp
                str/split-lines) )

(def width (count (first lines)))
(def rows (count lines))
(def input (str/join "" lines))
(def input-len (count input))

(def empty-val \L)
(def floor-val \.)
(def occupied-val \#)

(defn empty'? [x] (= x empty-val))
(defn occupied? [x] (= x occupied-val))

(defn left-edge? [pos] (zero? (mod pos 10)))
(defn right-edge? [pos] (left-edge? (inc pos)))

(defn above [pos] (when pos (if (> pos width) (- pos width))))
(defn below [pos] (when pos (if (>= (+ pos width) input-len) nil (+ pos width))))
(defn left [pos] (if (and pos (not (left-edge? pos)) (> pos 0)) (dec pos)))
(defn right [pos] (if (and pos (not (right-edge? pos)) (< pos (dec input-len))) (inc pos)))

(defn adjacent-vals [input pos]
  (->> [(left pos) (right pos) (above pos) (below pos) (above (left pos)) (above (right pos)) (below (left pos)) (below (right pos))]
       (remove nil?)
       (map (partial nth input))))

(defn count-occupied-adjacent [input pos]
  (count (filter occupied? (adjacent-vals input pos))))

(defn apply-rules [input pos]
  (let [val (nth input pos)]
    (cond
      (and (empty'? val) (zero? (count-occupied-adjacent input pos)))  occupied-val
      (and (occupied? val) (>= (count-occupied-adjacent input pos) 4)) empty-val
      :else                                                            val)))

(defn step [input]
  (mapv (partial apply-rules input) (range input-len)))

(prn :part-1
     (->> (iterate step input)
          (partition 2 1)
          (filter (partial apply =))
          ffirst
          (filter #{occupied-val})
          count))
