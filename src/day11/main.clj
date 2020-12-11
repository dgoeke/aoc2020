(ns aoc.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def lines (->> "day11-input.txt" io/resource slurp str/split-lines))

(def width (count (first lines)))
(def input (str/join "" lines))    ; input is a flat list, not 2 dimensional
(def input-len (count input))

(defn empty'? [x] (= x \L))
(defn floor? [x] (= x \.))
(defn occupied? [x] (= x \#))

(defn left-edge? [pos] (zero? (mod pos width)))
(defn right-edge? [pos] (left-edge? (inc pos)))
(defn above [pos] (if (and pos (>= pos width)) (- pos width)))
(defn below [pos] (if (and pos (< (+ pos width) input-len)) (+ pos width)))
(defn left [pos] (if (and pos (not (left-edge? pos))) (dec pos)))
(defn right [pos] (if (and pos (not (right-edge? pos))) (inc pos)))

(defn look [input pos max-distance direction-fn]
  (let [[dist result] (some->> (iterate direction-fn pos)
                               (map-indexed vector)
                               rest
                               (drop-while (fn [[_ val]]
                                             (when val (floor? (nth input val)))))
                               first)]
    (if (or (nil? result) (and max-distance (> dist max-distance)))
      \.
      (nth input result))))

(defn count-occupied [input max-distance pos]
  (let [iterators [left right above below
                   (comp above left) (comp above right) (comp below left) (comp below right)]]
    (->> iterators
         (map (partial look input pos max-distance))
         (filter occupied?)
         count)))

(defn apply-rules [input vision-distance occupied-threshold pos]
  (let [val (nth input pos)]
    (cond
      (and (empty'? val) (zero? (count-occupied input vision-distance pos)))   \#
      (and (occupied? val)
           (>= (count-occupied input vision-distance pos) occupied-threshold)) \L
      :else                                                                    val)))

(defn step [count-occupied-fn occupied-threshold input]
  (mapv (partial apply-rules input count-occupied-fn occupied-threshold)
        (range input-len)))

(defn final-count [input vision-distance occupied-threshold]
  (->> (iterate (partial step vision-distance occupied-threshold) input)
       (partition 2 1)
       (filter (partial apply =))
       ffirst
       (filter #{\#})
       count))

(prn :part-1 (final-count input 1 4))
(prn :part-2 (final-count input nil 5))
