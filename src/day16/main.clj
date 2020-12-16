(ns aoc.day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-field [line]
  (let [[_ n v1 v2 v3 v4] (re-matches #"^(.*?): (\d+)-(\d+) or (\d+)-(\d+)$" line)]
    [n #(or (<= (Integer. v1) % (Integer. v2)) (<= (Integer. v3) % (Integer. v4)))]))

(defn parse-ticket-line [line]
  (into [] (map #(Integer. %) (str/split line #","))))

(defn parse-lines [input]
  (-> input
      (update :fields (comp (partial into {}) (partial map parse-field)))
      (update :mine #(parse-ticket-line (second %)))
      (update :nearby #(map parse-ticket-line (rest %)))))

(def data
  (->>  "day16-input.txt" io/resource slurp str/split-lines
        (partition-by str/blank?)
        (remove (comp empty? first))
        (map vector [:fields :mine :nearby])
        (into {})
        (parse-lines)))

(defn invalid-values [fields ticket]
  (filter (comp not (apply some-fn (vals fields))) ticket))

(prn :part-1
     (->> (:nearby data)
          (map (partial invalid-values (:fields data)))
          (remove empty?)
          (flatten)
          (apply +)))

(prn :part-2
     (let [valid-tickets (filterv (comp empty? (partial invalid-values (:fields data))) (:nearby data))
           valid-col?    (fn [field column] (every? ((:fields data) field)
                                                    (map #(nth % column) valid-tickets)))
           valid-matrix  (for [field (keys (:fields data))
                               col   (range (count (:fields data)))]
                           [field col (valid-col? field col)])
           valid-map     (->> valid-matrix
                              (filter #(nth % 2))
                              (reduce (fn [acc [class row _]] (update acc class (fnil conj #{}) row)) {}))
           assignments   (loop [remaining valid-map
                                assigned  {}]
                           (let [[field-name col-set] (first (filter #(= 1 (count (second %))) remaining))
                                 col                  (first col-set)]
                             (if (nil? col)
                               assigned
                               (recur (->> (dissoc remaining field-name)
                                           (map (fn [[k v]] [k (disj v col)]))
                                           (into {}))
                                      (assoc assigned field-name col)))))]
       (->> assignments
            (filter (fn [[k _]] (str/starts-with? k "departure")))
            (map second)
            (map #((:mine data) %))
            (apply *))))

