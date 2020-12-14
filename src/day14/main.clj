(ns aoc.day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (->> "day14-input.txt"
       io/resource slurp
       str/split-lines))

(def sample-input
  '("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    "mem[8] = 11"
    "mem[7] = 101"
    "mem[8] = 0"))

(defn parse-line [input-line]
  (let [mask-stmt (re-matches #"^mask = ([10X]+)$" input-line)
        assign-stmt (re-matches #"^mem\[(\d+)\] = (\d+)$" input-line)]
    (cond
      mask-stmt [:set-mask (second mask-stmt)]
      assign-stmt [:assign (map #(Integer. %) (rest assign-stmt))])))

(defn with-mask [mask val]
  (let [val-str    (Integer/toBinaryString val)
        val-str    (apply str (concat (repeat (- (count mask) (count val-str)) "0") val-str))
        mask       (map #(if (= % \X) false %) mask)
        result-str (->> (map vector mask val-str)
                        (map (partial some identity))
                        (apply str))]
    (Long/parseLong result-str 2)))

(defmulti step (fn [_acc [inst _val]] inst))
(defmethod step :set-mask [state [_ new-mask]] (assoc state :mask new-mask))
(defmethod step :assign [{:as state :keys [mask]} [_ [n v]]]
  (update state :registers assoc n (with-mask mask v)))

(defn execute [program-lines]
  (->> program-lines
       (map parse-line)
       (reduce step {:mask (apply str (repeat 36 \X)) :registers {}})))

(prn :part-1 (->> (execute input) :registers vals (apply +)))

