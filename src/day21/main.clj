(ns aoc.day21
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(->> "day21-input.txt" io/resource slurp str/split-lines)

(def input (->> "day21-input.txt" io/resource slurp
                (re-seq #"[^\n]+")
                (map #(let [[is _ as] (->> (re-seq #"\w+" %) (partition-by #{"contains"}))]
                        [(set is) (set as)]))))

(def candidates
  (for [allergen (reduce set/union (map second input))]
    [allergen (reduce set/intersection (keep #(if ((second %) allergen) (first %)) input))]))

(def ingredients-with-allergen
  (reduce set/union (map second candidates)))

(prn :part-1
     (reduce + (map #(count (set/difference (first %) ingredients-with-allergen)) input)))

(prn :part-2
     (loop [candidates candidates
            result     {}]
       (if-not (seq candidates)
         (->> result sort (map second) (str/join ","))
         (let [found-mapping     (keep (fn [[allergen ingredients]]
                                         (if (= 1 (count ingredients)) [allergen (first ingredients)]))
                                       candidates)
               found-ingredients (set (map second found-mapping))
               new-candidates    (keep (fn [[allergen ingredients]]
                                         (let [new-ingredients (set/difference ingredients found-ingredients)]
                                           (if (seq new-ingredients)
                                             [allergen new-ingredients])))
                                       candidates)]
           (recur new-candidates (into result found-mapping))))))
