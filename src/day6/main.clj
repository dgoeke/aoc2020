(ns aoc.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input
  (-> "day6-input.txt"
      io/resource
      slurp
      str/split-lines
      (->>
       (partition-by (partial = ""))  ; create subsequences that begin/end at the empty lines
       (remove (partial = '(""))))))  ; remove empty subseqs (corresponding to blank lines)

(prn :part-1
     (->> input
          (map (partial reduce (partial reduce conj) #{}))
          (map count)
          (apply +)))

(prn :part-2
     (->> input
          (map (partial map (partial into #{})))
          (map (partial apply set/intersection))
          (map count)
          (apply +)))

