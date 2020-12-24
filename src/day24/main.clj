(ns aoc.day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def walk-transform
  {"e"  [inc dec identity]
   "w"  [dec inc identity]
   "nw" [identity inc dec]
   "se" [identity dec inc]
   "ne" [inc identity dec]
   "sw" [dec identity inc]})

(defn coords-for [walk-seq]
  (reduce (fn [[x y z] [tx ty tz]] [(tx x) (ty y) (tz z)]) [0 0 0]
          (map walk-transform walk-seq)))

(def input
  (->> "day24-input.txt" io/resource slurp str/split-lines
       (map (partial re-seq #"(w|e|ne|nw|se|sw)?+"))
       (map (partial filter (comp not nil? second)))
       (map (partial map first))
       (map coords-for)))

(prn :part-1 (->> (reduce (fn [acc coord] (update acc coord (fnil not false))) {} input)
                  vals (filter identity) count))

(defn neighbors [[x y z]]
  [[(inc x) (dec y) z] [(inc x) y (dec z)] [x (inc y) (dec z)]
   [(dec x) (inc y) z] [(dec x) y (inc z)] [x (dec y) (inc z)]])

(defn count-b-neighbors [board coord]
  (->> (neighbors coord) (map board) (filter identity) count))

(defn step [board]
  (let [b-squares     (remove #(let [n (count-b-neighbors board %)] (or (zero? n) (> n 2))) board)
        new-b-squares (->> (mapcat neighbors board) (filter #(= 2 (count-b-neighbors board %))))]
    (into #{} (concat b-squares new-b-squares))))

(prn :part-2 (let [input-board (reduce (fn [acc coord] (update acc coord (fnil not false))) {} input)
                   board       (->> input-board (filter (comp identity second)) (map first) (into #{}))]
               (->> (iterate step board)
                    (drop 100)
                    first count)))
