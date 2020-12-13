(ns aoc.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (->> "day12-input.txt"
       io/resource slurp
       str/split-lines
       (map (partial re-matches #"^([NSEWLRF])(\d+)$"))
       (map rest)
       (map (fn [[d v]] [(keyword d) (Integer. v)]))))

(def dirs [:E :S :W :N]) ; clockwise with east at index 0

(defn step-part-1 [{:as state :keys [x y dir]} [d v]]
  (condp = d
    :N (update state :y (partial + v))
    :S (update state :y #(- % v))
    :E (update state :x (partial + v))
    :W (update state :x #(- % v))
    :F (step-part-1 state [(dirs dir) v])
    :L (update state :dir #(mod (- % (/ v 90)) 4))
    :R (update state :dir #(mod (+ % (/ v 90)) 4))))

(defn right-90 [[x y]] [y (- x)])  ; rotate [x y] by 90 degrees clockwise
(defn left-90 [[x y]] [(- y) x])   ; rotate [x y] by 90 degrees counter-clockwise
(defn set-xy [state [x y]] (-> state (assoc :x x) (assoc :y y)))

(defn step-part-2 [{:as state :keys [x y my-x my-y]} [d v]]
  (condp = d
    :R (->> (iterate right-90 [x y]) (drop (/ v 90)) first (set-xy state))
    :L (->> (iterate left-90 [x y]) (drop (/ v 90)) first (set-xy state))
    :F (-> state
           (update :my-x (partial + (* v x)))
           (update :my-y (partial + (* v y))))
    (step-part-1 state [d v])))

(prn :part-1 (->> input
                  (reduce step-part-1 {:x 0 :y 0 :dir 0})
                  ((juxt :x :y)) (map #(Math/abs %)) (reduce +)))

(prn :part-2 (->> input
                  (reduce step-part-2 {:x 10 :y 1 :my-x 0 :my-y 0})
                  ((juxt :my-x :my-y)) (map #(Math/abs %)) (reduce +)))
