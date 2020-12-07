(ns aoc.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn bag-rule [text]
  (let [[_ num color] (re-matches #"^(\d+) (.*?) bags?$" text)]
    {:num   (Integer. num)
     :color color}))

(defn line-rule [acc text]
  (let [[_ color contents] (re-matches #"(.*?) bags contain (.*?)." text)]
    (assoc acc color (if (= contents "no other bags")
                       '()
                       (->> (str/split contents #",")
                            (map str/trim)
                            (map bag-rule))))))

(def rules
  (reduce line-rule {}
          (-> "day7-input.txt"
              io/resource
              slurp
              str/split-lines)))

(def can-contain?
  (memoize
   (fn [bag target]
     (let [sub-bags (map :color (get rules bag))
           direct   (some? (some #{target} sub-bags))
           results  (map #(can-contain? % target) sub-bags)]
       (or direct (some true? results))))))

(defn count-contents [bag]
  (let [sub-bags (get rules bag)]
    (if (empty? sub-bags)
      0
      (reduce (fn [acc {:keys [color num]}]
                (+ acc num (* num (count-contents color))))
              0
              sub-bags))))

(prn :part-1 (count (filter #(can-contain? % "shiny gold") (keys rules))))

(prn :part-2 (count-contents "shiny gold"))
