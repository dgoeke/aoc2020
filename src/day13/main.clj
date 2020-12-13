(ns aoc.day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

(def input-lines (->> "day13-input.txt" io/resource slurp str/split-lines))

(defn read-schedule [input]
  (->> (str/split input #",") (remove (partial = "x")) (map read-string)))

(defn first-after [ts schedule]
  (map (fn [start] [start (->> start
                               (iterate (partial + start))
                               (drop-while (partial > ts))
                               first
                               (#(- % ts)))])
       schedule))

(prn :part-1 (let [[timestamp buses] input-lines]
               (->> (first-after (Integer. timestamp) (read-schedule buses))
                    (sort-by second)
                    first
                    (apply *))))

;; part 2: find x such that
;;
;; x       % 7  = 0   implies   x %  7 =  0
;; (x + 1) % 13 = 0   implies   x % 13 = 12
;; (x + 4) % 59 = 0   implies   x % 59 = 55
;; (x + 6) % 31 = 0   implies   x % 31 = 25
;; (x + 7) % 19 = 0   implies   x % 19 = 12   -> time for the CRT

(defn read-schedule-part-2 [input]
  (->> (str/split input #",")
       (map-indexed vector)
       (remove (fn [[_ x]] (= x "x")))
       (map (fn [[offset x]] [offset (Integer. x)]))))

;; extended Euclidean algorithm - return GCD and Bézout coefficients
(defn extended-gcd [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else     (loop [s 0 s0 1 t 1 t0 0 r (math/abs b) r0 (math/abs a)]
                    (if (zero? r)
                      [r0 s0 t0]
                      (let [q (quot r0 r)]
                        (recur (- s0 (* q s)) s
                               (- t0 (* q t)) t
                               (- r0 (* q r)) r))))))

(defn chinese-remainder [ns as]
  (let [prod (apply * ns)
        f    (fn [sum [n_i a_i]]
               (let [p     (quot prod n_i)
                     inv_p (second (extended-gcd p n_i))]
                     (+ sum (* a_i inv_p p))))]
    (mod (reduce f 0 (map vector ns as)) prod)))

(prn :part-2 (let [[_ buses]   input-lines
                   schedule    (read-schedule-part-2 buses)
                   congruences (map (fn [[a b]] [b (mod (- b a) b)]) schedule)]
               (chinese-remainder (map first congruences) (map second congruences))))
