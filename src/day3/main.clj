(ns aoc.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (-> "day3-input.txt"
      io/resource
      slurp
      str/split-lines))

(defn tree? [x] (= x \#))

(defn sloper
  "Reducing function accepts an accumulator and a row"
  [{:as accum :keys [pos tree-count right down skip?]} row]
  (if skip?
    (assoc accum :skip? false)
    (let [pos   (mod pos (count row))
          splat (if (tree? (nth row pos)) 1 0)]
      (-> accum
          (assoc :pos (+ pos right))
          (assoc :skip? (> down 1))
          (update :tree-count (partial + splat))))))

(defn init-sloper [right down]
  {:pos        0
   :tree-count 0
   :right      right
   :down       down
   :skip?      false})

(defn check-slope [[right down]]
  (reduce sloper (init-sloper right down) input))

(def part-2-slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(prn :part-1 (check-slope [3 1]))

(prn :part-2 (->> part-2-slopes
                  (map check-slope)
                  (map :tree-count)
                  (apply *)))
