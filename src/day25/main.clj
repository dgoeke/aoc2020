(ns aoc.day25
  (:require [clojure.string :as str]))

(def input [19774466 7290641])

(defn crack [val]
  (loop [i 0 n 1]
    (if (= n val) i (recur (inc i) (mod (* 7 n) 20201227)))))

(defn transform [subj loop]
  (reduce #(mod (* %1 %2) 20201227) 1 (repeat loop subj)))

(defn solve [[n1 n2]]
  (transform n2 (crack n1)))

(prn :part-1 (solve input))
