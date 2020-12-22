(ns aoc.day22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def hands (-> "day22-input.txt" io/resource slurp
               (str/split #"\r\n\r\n")
               (->> (map (comp rest str/split-lines))
                    (map (partial map read-string)))))

(defn score [cards]
  (reduce + (map-indexed #(* (inc %1) %2) (reverse cards))))

(defn play [[x & xs] [y & ys]]
  (cond
    (nil? x) (score (cons y ys))
    (nil? y) (score (cons x xs))
    :else    (if (> x y)
            (recur (concat xs [x y]) ys)
            (recur xs (concat ys [y x])))))

(defn play-rec [xs ys seen sub?]
  (let [[x & xs*] xs
        [y & ys*] ys
        seen*     (conj seen [xs ys])]
    (cond
      (contains? seen [xs ys]) (if sub? 1 (score xs))
      (nil? x)                 (if sub? 2 (score ys))
      (nil? y)                 (if sub? 1 (score xs))
      (or (< (count xs*) x)
          (< (count ys*) y))   (if (> x y)
                                 (recur (concat xs* [x y]) ys* seen* sub?)
                                 (recur xs* (concat ys* [y x]) seen* sub?))
      :else                    (let [subgame (play-rec (take x xs*) (take y ys*) #{} true)]
                                 (if (= 1 subgame)
                                   (recur (concat xs* [x y]) ys* seen* sub?)
                                   (recur xs* (concat ys* [y x]) seen* sub?))))))

(prn :part-1 (apply play hands))
(prn :part-2 (play-rec (first hands) (second hands) {} false))
