(ns aoc.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input
  (->> "day9-input.txt"
       io/resource
       slurp
       str/split-lines
       (mapv read-string)))

(defn has-pairwise-sum? [xs target]
  (let [all-sums (set (for [n1 xs n2 xs] (+ n1 n2)))]
    (some? (all-sums target))))

(def part1
  (let [preamble 25
        parts    (partition (inc preamble) 1 input)]
    (->> parts
         (filter #(not (has-pairwise-sum? (take preamble %) (last %))))
         (map last)
         first)))

(defn all-subseqs [xs]
  (for [x (range (count xs))
        y (range (count xs)) :when (> y (inc x))]
    (subvec xs x y)))

(prn :part-1 part1)
(prn :part-2
     (->> (all-subseqs input)
          (filter #(= part1 (reduce + %)))
          (map #(+ (apply max %) (apply min %)))
          first))


