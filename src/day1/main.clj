;; Before you leave, the Elves in accounting just need you to fix your
;; expense report (your puzzle input); apparently, something isn't quite
;; adding up. Specifically, they need you to find the two entries that
;; sum to 2020 and then multiply those two numbers together.

(ns aoc.day1
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def input
  (-> "day1-input.edn"
      io/resource
      slurp
      edn/read-string
      sort))

(defn find-target-sum [sorted-input target]
  (loop [increasing sorted-input
         decreasing (reverse sorted-input)]
    (let [start  (first increasing)
          end    (first decreasing)
          result (+ start end)]
      (cond
        (= result target) [start end]  ; got it
        (= start end)     nil          ; no answer found
        (> result target) (recur increasing (rest decreasing))
        :else             (recur (rest increasing) decreasing)))))

(defn find-target-sum-triplet [sorted-input target]
  (loop [items sorted-input]
    (let [item       (first items)
          new-target (- target item)
          result      (find-target-sum sorted-input new-target)]
      (cond
        (nil? item)   nil
        (nil? result) (recur (rest items))
        :else         [item (first result) (second result)]))))

(prn :part-1 (apply * (find-target-sum input 2020)))
(prn :part-2 (apply * (find-target-sum-triplet input 2020)))
