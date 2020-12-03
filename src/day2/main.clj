(ns aoc.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def line-regex #"(\d+)-(\d+) (\w): (.*)")

(defn parse-line [line]
  (let [[_ min max ch pw] (re-matches line-regex line)]
    {:min (Integer. min)
     :max (Integer. max)
     :ch  (first ch)
     :pw  pw}))

(def input
  (->> "day2-input.txt"
       io/resource
       slurp
       str/split-lines
       (map parse-line)))

(defn valid-pw-part1? [{:keys [min max ch pw]}]
  (let [count (get (frequencies pw) ch)]
    (and (not (nil? count)) (<= min count) (>= max count))))

(defn valid-pw-part2? [{:keys [min max ch pw]}]
  (let [fst (= ch (nth pw (dec min)))
        snd (= ch (nth pw (dec max)))]
    (or (and fst (not snd))
        (and snd (not fst)))))

(prn :part-1 (->> input (filter valid-pw-part1?) count))
(prn :part-2 (->> input (filter valid-pw-part2?) count))
