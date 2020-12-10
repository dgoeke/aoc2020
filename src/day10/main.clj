(ns aoc.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (letfn [(add-my-adapter [jolts] (conj jolts (+ 3 (first jolts))))]
    (->> "day10-input.txt"
         io/resource slurp
         str/split-lines
         (mapv read-string)   ; read all values
         (concat [0])         ; add the 0-jolt wall adapter
         sort
         reverse
         add-my-adapter)))    ; add my +3j charger

(prn :part-1
     (->> input
          (partition 2 1)          ; examine all sorted pairs
          (map (partial reduce -)) ; take their difference
          frequencies              ; count number of 1s and 3s
          vals
          (apply *)))

(defn find-connections [all-items item]
  [item (->> all-items
             (filter #(let [diff (- % item)] (<= 1 diff 3))))])

(def count-paths
  (memoize (fn [g source target]
             (if (= source target)
               1
               (reduce + (flatten (for [s (get g source)]
                                    (count-paths g s target))))))))

(prn :part-2
     (let [target (apply max input)
           graph  (into {} (map (partial find-connections input) input))]
       (count-paths graph 0 target)))
