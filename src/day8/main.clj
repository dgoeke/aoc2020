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
       (map #(update % 1 read-string)) ; instructions looks like:
       vec))                           ;  [[:nop 1] [:jmp -3] ...]

;; step executes the next instruction in the program. it accepts a state
;; and returns a state, so (step (step (step (step state)))) executes
;; repeated instructions
(defn step [{:as state :keys [instructions ip last-ip acc seen result]}]
  (cond
    (= ip (count instructions)) (assoc state :result :terminated)
    (seen ip)                   (assoc state :result :looped)
    result                      state
    :else
    (let [[inst arg] (nth instructions ip)
          new-state  (-> state
                         (update :seen conj ip)
                         (assoc :last-ip ip))]
      (condp = inst
        :nop (update new-state :ip inc)
        :acc (-> new-state (update :ip inc) (update :acc + arg))
        :jmp (update new-state :ip + arg)))))

;; execute repeatedly iterates the `step` fn until it terminates or loops
(defn execute [instructions]
  (let [state {:ip 0 :last-ip 0 :acc 0 :seen #{} :instructions instructions}]
    (->> (iterate step state)
         (drop-while (comp nil? :result))
         first)))

;; patch-it patches every jmp/nop and executes them in order until it doesn't loop
;;  `patches` contains a list of offsets/instructions to patch: [[0 :jmp] [7 :nop] ...]
;;  `patched-programs` is a list of new programs corresponding to each patch
(defn patch-it []
  (let [patches          (->> instructions
                              (map-indexed vector)
                              (filter (fn [[idx [inst _]]] (not= :acc inst)))
                              (map (juxt first (comp first second))))
        patch-fn         (fn [offset inst]
                           (update instructions offset assoc 0 (if (= inst :jmp) :nop :jmp)))
        patched-programs (map (partial apply patch-fn) patches)]
    (->> patched-programs
         (map execute)
         (drop-while #(= (:result %) :looped))
         first)))

(prn :part-1 (:acc (execute instructions)))
(prn :part-2 (:acc (patch-it)))
