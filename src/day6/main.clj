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
       (partition-by (partial = ""))  ; input is a list of strings per group:
       (remove (partial = '(""))))))  ;  (("abc" "def" "adf") ("bcd" "bd"))

(prn :part-1       
     (->> input                      
          (map (partial reduce (partial reduce conj) #{}))  ; merge strings into one set per group
          (map count)
          (apply +)))

(prn :part-2
     (->> input
          (map (partial map (partial into #{})))  ; convert each string to a set
          (map (partial apply set/intersection))  ; intersect the sets for each group
          (map count)
          (apply +)))

