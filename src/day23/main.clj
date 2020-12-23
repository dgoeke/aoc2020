(ns aoc.day23
  (:require [clojure.string :as str]))

(defn play [cards n-cups n-turns]
  (let [num-labels (count cards)
        cups       (concat cards (map inc (range (count cards) n-cups)))
        links      (long-array n-cups)
        cup-after  (fn ^long [^long n] (aget links n))]
    (doseq [[^long c1 ^long c2] (->> cups
                                     (map dec)
                                     (partition 2 1 [(dec (first cards))]))]
      (aset links c1 c2))
    (loop [cur (dec (first cards))
           n   0]
      (if (== n n-turns)
        (->> (iterate cup-after 0)
             (take n-cups)
             (map inc))
        (let [c1      (cup-after cur)
              c2      (cup-after c1)
              c3      (cup-after c2)
              next'   (cup-after c3)
              label   (loop [tgt (mod (dec cur) n-cups)]
                        (if (#{c1 c2 c3} tgt)
                          (recur (mod (dec tgt) n-cups))
                          tgt))
              post-c3 (cup-after label)]
          (aset links (int cur) (long next'))
          (aset links (int label) (long c1))
          (aset links (int c3) (long post-c3))
          (recur next' (inc n)))))))

(def starting-hand '(8 5 3 1 9 2 6 4 7))

(prn :part-1 (->> (play starting-hand 9 100) rest (map str) str/join Long/parseLong))

(prn :part-2 (->> (play starting-hand (* 1000 1000) (* 10 1000 1000)) rest (take 2) (apply *)))
