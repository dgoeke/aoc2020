(ns aoc.day20
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def tiles
  (->> "day20-input.txt" io/resource slurp str/split-lines
       (partition-by str/blank?)
       (remove (partial = '("")))
       (map (fn [[title & rows]] [(-> (re-matches #"Tile (\d+):" title) second read-string) (vec rows)]))
       (into {})))

(def size (int (Math/sqrt (count tiles))))

(defn signature [edge]
  (let [binary (map {\. \0 \# \1} edge)]
    (Long/parseLong (apply str binary) 2)))

(defn variants [tile]
  (let [r90 #(->> % (apply mapv vector) reverse vec (mapv (partial apply str)))]
    (vec (concat (take 4 (iterate r90 (reverse tile)))
                 (take 4 (iterate r90 tile))))))

(defn annotate-edges [id tile]
  {:piece id
   :tile  tile
   :edges {:N (signature (first tile))
           :S (signature (last tile))
           :W (signature (map first tile))
           :E (signature (map last tile))}})

(def tile-variants
  (let [tile-variants (->> tiles
                           (mapcat (fn [[id v]] (->> v variants (mapv (partial annotate-edges id)))))
                           (map-indexed vector)
                           (map (fn [[id t]] (assoc t :variant id))))
        conn-fn       (fn [edges d1 d2] (->> tile-variants
                                             (filter (fn [t] (= (d1 edges) (d2 (:edges t)))))
                                             (map :variant) set))]
    (map (fn [{:as tile :keys [variant edges]}]
           (-> tile
               (update-in [:conns :N] (fnil set/union #{}) (conn-fn edges :N :S))
               (update-in [:conns :S] (fnil set/union #{}) (conn-fn edges :S :N))
               (update-in [:conns :E] (fnil set/union #{}) (conn-fn edges :E :W))
               (update-in [:conns :W] (fnil set/union #{}) (conn-fn edges :W :E))))
         tile-variants)))

(defn next-tile [[y x]]
  (cond
    (< x (dec size)) [y (inc x)]
    (< y (dec size)) [(inc y) 0]
    :else            nil))

(def debug (atom 0))

(defn try-assign [result unseen [y x]]
  (if (empty? unseen) result
      (let [valid-above  (some-> result (get-in [(dec y) x]) (->> (nth tile-variants)) :conns :S)
            valid-left   (some-> result (get-in [y (dec x)]) (->> (nth tile-variants)) :conns :E)
            candidates   (cond (and (nil? valid-above) (nil? valid-left)) (range (count tile-variants))
                               (nil? valid-above)                         valid-left
                               (nil? valid-left)                          valid-above
                               :else                                      (set/intersection valid-left valid-above))
            next-pos     (next-tile [y x])
            tiles-to-try (filter (comp unseen :piece (partial nth tile-variants)) candidates)]
        (loop [tile-list tiles-to-try]
          (when-let [tile-id (first tile-list)]
            (or (try-assign (assoc-in result [y x] tile-id)
                            (disj unseen (:piece (nth tile-variants tile-id)))
                            next-pos)
                (recur (rest tile-list))))))))

(def solution
  (let [rows (vec (repeatedly size #(vec (repeat size nil))))]
    (try-assign rows (set (keys tiles)) [0 0])))

(prn :part-1 (->> [[0 0] [0 (dec size)] [(dec size) 0] [(dec size) (dec size)]]
                  (map (comp :piece (partial nth tile-variants) (partial get-in solution)))
                  (apply *)))

(def monster ["                  # "
              "#    ##    ##    ###"
              " #  #  #  #  #  #   "])

(def monster-len (-> monster first count))

(defn monster-at? [lines x y]
  (every? false?
          (for [x' (range x (+ x monster-len))
                y' (range y (+ y 3))]
            (if (= \# (get-in monster [(- y' y) (- x' x)]))
              (not= \# (get-in lines [y' x']))
              false))))

(defn scan-for-monsters [lines]
  (let [line-len (-> lines first count)]
    (->> (for [y (range (count lines))
              x (range (- line-len monster-len))]
          [y x (monster-at? lines x y)])
         (filter (comp true? #(nth % 2))))))

(prn :part-2
     (let [full-field (->> solution
                           (map (partial map (comp :tile (partial nth tile-variants))))
                           (map (partial map (fn [lines] (subvec (vec (map #(subs % 1 9) lines)) 1 9))))
                           (map (partial apply map vector))
                           (map (partial map (partial apply str)))
                           flatten)
           count-#s   #(->> % (apply concat) (filter #{\#}) count)
           monsters   (->> (variants full-field)
                           (map scan-for-monsters)
                           (apply concat)
                           count)]
       (- (count-#s full-field) (* monsters (count-#s monster)))))
