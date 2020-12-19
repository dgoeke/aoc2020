(ns aoc.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

(defn build-parser [rule-list]
  (insta/parser (->> (sort rule-list)
                     (map #(str/replace % #": " " = "))
                     (map #(str/replace % #"\"" "'"))
                     (str/join "\n"))) )

(def part1-input (->> "day19-input.txt" io/resource slurp))
(def part2-input (-> part1-input
                     (str/replace #"\s8: 42" "\n8: 42 | 42 8")
                     (str/replace #"\s11: 42 31" "\n11: 42 31 | 42 11 31")))

(defn count-matches [raw-input]
  (let [[rules msgs] (split-with (comp not str/blank?)
                                 (str/split-lines raw-input))
        parser       (build-parser rules)]
    (->> (rest msgs)
         (filter (comp vector? (partial insta/parse parser)))
         count)))

(prn :part-1 (count-matches part1-input))
(prn :part-2 (count-matches part2-input))
