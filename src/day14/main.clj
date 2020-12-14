(ns aoc.day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (->> "day14-input.txt" io/resource slurp str/split-lines))

(defn parse-line [input-line]
  (let [mask-stmt   (re-matches #"^mask = ([10X]+)$" input-line)
        assign-stmt (re-matches #"^mem\[(\d+)\] = (\d+)$" input-line)]
    (cond
      mask-stmt   [:set-mask (second mask-stmt)]
      assign-stmt [:assign (map #(Integer. %) (rest assign-stmt))])))

;; for part 1, use wild-char \X. for part 2, wild-char \0
(defn with-mask [wild-char mask val]
  (let [val-str    (Integer/toBinaryString val)
        val-str    (apply str (concat (repeat (- (count mask) (count val-str)) "0") val-str))
        mask       (map #(if (= % wild-char) false %) mask)]
    (->> (map vector mask val-str)
         (map (partial some identity))
         (apply str))))

(defmulti step (fn [_acc [inst _val]] inst))
(defmethod step :set-mask [state [_ new-mask]] (assoc state :mask new-mask))
(defmethod step :assign [{:as state :keys [mask]} [_ [n v]]]
  (update state :registers assoc n (Long/parseLong (with-mask \X mask v) 2)))

(defn execute [program-lines]
  (->> program-lines
       (map parse-line)
       (reduce step {:mask (apply str (repeat 36 \X)) :registers {}})))

(prn :part-1 (->> (execute input) :registers vals (apply +)))

;; (all-bit-values 2) => '( (0 0) (0 1) (1 0) (1 1) )
(defn all-bit-values [n-bits]
  (let [pad-zeros (fn [x] (apply str (concat (repeat (- n-bits (count x)) "0") x)))]
    (->> (range (Math/pow 2 n-bits))
         (map #(Integer/toBinaryString %))
         (map pad-zeros)
         (map (partial map #(- (int %) (int \0)))))))

;; (apply-bits-to-mask "0X1X" '(1 0)) => "0110" => 6
(defn apply-bits-to-mask [mask bits]
  (let [reducer    (fn [[result bits] mask-char]
                     (if (= mask-char \X)
                       [(conj result (first bits)) (rest bits)]
                       [(conj result (- (int mask-char) (int \0))) bits]))
        result-str (apply str (first (reduce reducer [[] bits] mask)))]
    (Long/parseLong result-str 2)))

;; (all-mask-values "X0X") => '(000 001 100 101) => (0 1 4 5)
(defn all-mask-values [mask]
  (let [bits (count (filter (partial #{\X}) mask))]
    (->> (all-bit-values bits)
         (map (partial apply-bits-to-mask mask)))))

(defmethod step :assign [{:as state :keys [mask]} [_ [n v]]]
  (reduce #(update %1 :registers assoc %2 v) state (all-mask-values (with-mask \0 mask n))))

(prn :part-2 (->> (execute input) :registers vals (apply +)))
