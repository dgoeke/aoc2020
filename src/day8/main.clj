(ns aoc.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def instructions
  (->> "day8-input.txt"
       io/resource
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map #(update % 0 keyword))
       (map #(update % 1 read-string))
       vec))

(defn execute [instructions]
  (loop [ip      0
         last-ip 0
         acc     0
         seen    #{}]
    (cond
      (= ip (count instructions)) {:stopped-at ip :last-ip :terminated :accumulator acc}
      (seen ip)                   {:stopped-at ip :last-ip last-ip :accumulator acc}
      :else
      (let [[inst arg] (nth instructions ip)]
        (condp = inst
          :nop (recur (inc ip) ip acc (conj seen ip))
          :acc (recur (inc ip) ip (+ acc arg) (conj seen ip))
          :jmp (recur (+ ip arg) ip acc (conj seen ip)))))))

(prn :part-1 (execute instructions))

(defn patch-instructions [index]
  (let [offset     (->> instructions
                        (drop index)
                        (take-while #(= (first %) :acc))
                        count)
        new-index (+ offset index)
        [inst arg] (nth instructions new-index)]
    [(inc new-index)
     (assoc instructions new-index [(if (= inst :jmp) :nop :jmp) arg])]))

(defn patch-it []
  (loop [index                0
         patched-instructions instructions]
    (let [{:as result :keys [last-ip accumulator]} (execute patched-instructions)]
      (if (= last-ip :terminated)
        result
        (let [[new-index next-patch] (patch-instructions index)]
          (recur new-index next-patch))))))

(prn :part-2 (patch-it))
