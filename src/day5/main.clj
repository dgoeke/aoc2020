(ns aoc.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn seat-finder [{:as acc :keys [row row-max col col-max]} instruction]
  (let [row-zone-size (/ (inc (- row-max row)) 2)
        col-zone-size (/ (inc (- col-max col)) 2)]
    (condp = instruction
      \B (update acc :row (partial + row-zone-size))
      \F (update acc :row-max #(- % row-zone-size))
      \R (update acc :col (partial + col-zone-size))
      \L (update acc :col-max #(- % col-zone-size)))))

(defn row-id [{:keys [row col]}] (-> row (* 8) (+ col)))

(def all-row-ids
  (-> "day5-input.txt"
      io/resource
      slurp
      str/split-lines
      (->>
       (map (partial reduce seat-finder {:row 0 :row-max 127 :col 0 :col-max 7}))
       (map row-id)
       sort)))

(prn :part-1 (last all-row-ids))

(prn :part-2 (->> (map vector all-row-ids (rest (cycle all-row-ids)))
                  (remove (fn [[x y]] (= y (inc x))))
                  ffirst
                  inc))
