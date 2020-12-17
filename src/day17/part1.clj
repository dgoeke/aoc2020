(ns aoc.day17-part1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sim-rounds 6)
(def input  (->> "day17-input.txt" io/resource slurp str/split-lines))
(def input-width (count (first input)))
(def input-length (count input))
(def width (+ input-width (* 2 sim-rounds)))
(def length (+ input-length (* 2 sim-rounds)))
(def depth  (inc (* 2 sim-rounds)))
(def space  (make-array Boolean/TYPE width length depth))

(let [layer (int (/ depth 2))
      start-x (- (/ width 2) (/ input-width 2))
      start-y (- (/ length 2) (/ input-length 2))]
  (doseq [y (range input-length)
          x (range input-width)]
    (aset space (+ x start-x) (+ y start-y) layer (= \# (get-in input [y x])))))

(defn neighbors [x y z]
  (->> (for [a (range -1 2)
             b (range -1 2)
             c (range -1 2)]
         [(+ x a) (+ y b) (+ z c)])
       (remove #{[x y z]})
       (filter (fn [[x y z]]
                 (and (>= x 0) (>= y 0) (>= z 0) (< x width) (< y length) (< z depth))))))

(defn count-active-neighbors [x y z]
  (->> (neighbors x y z)
       (filter (partial apply aget space))
       count))

(defn step [changes [x y z]]
  (let [val         (aget space x y z)
        num-friends (count-active-neighbors x y z)]
    (cond
      (and (not val) (= 3 num-friends))    (conj changes [x y z true])
      (and val (and (not= 2 num-friends)
                    (not= 3 num-friends))) (conj changes [x y z false])
      :else                                changes)))

(defn apply-changes! [changes]
  (doseq [params changes]
    (apply aset space params)))

(dotimes [n sim-rounds]
  (let [every-coord (for [x (range width) y (range length) z (range depth)] [x y z])
        changes     (reduce step [] every-coord)
        _           (apply-changes! changes)]
    (prn :round (inc n) :count (->> every-coord (map (partial apply aget space)) (filter identity) count))))
