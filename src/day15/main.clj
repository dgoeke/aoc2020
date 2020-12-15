(ns aoc.day15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input '(11 0 1 10 5 19))

(defn step [{:as state :keys [turn]} n]
  (let [update-fn #(take 2 (-> % (conj turn) sort reverse))]
    (-> state
        (update-in [:seen n] update-fn)
        (assoc :last-n n)
        (update :turn inc))))

(defn next-n [{:as state :keys [last-n turn seen]}]
  (let [seen-last (seen last-n)]
    (if (= 1 (count seen-last))
      0
      (apply - seen-last))))

(defn to-turn [turn input]
  (let [state   (reduce step {:seen {} :turn 1} input)
        step-fn #(step % (next-n %))]
    (->> (iterate step-fn state)
         (drop-while #(<= (:turn %) turn))
         first :last-n)))

(prn :part-1 (to-turn 2020 input))

(time (prn :part-2 (to-turn 30000000 input)))  ; 54 seconds


