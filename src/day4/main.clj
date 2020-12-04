(ns aoc.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; given a seq:  ("hgt:183cm"  "pid:368895060"  "ecl:oth"  "eyr:2020")
;; return a map: {:hgt "183cm" :pid "368895060" :ecl "oth" :eyr "2020"}
(defn create-record [items]
  (->> items
       (map #(str/split % #":"))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(def input
  (-> "day4-input.txt"
      io/resource
      slurp
      str/split-lines                ; break file into one string per line
      (->>
       (mapcat #(str/split % #" "))  ; break "a:b c:d e:f" into "a:b" "c:d" "e:f"
       (partition-by (partial = "")) ; create subsequences that begin/end at the empty lines
       (remove (partial = '("")))    ; remove empty subseqs (corresponding to blank lines)
       (map create-record))))        ; convert each subseq to a map

;; a valid passport is defined as having these required and optional fields
(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

;; check how many passports meet the definition
(prn :part-1 (->> input
                  (map (partial s/valid? ::passport))
                  (remove false?)
                  count))

;; add new validations for each required field
(s/def ::height-cm (s/and #(str/ends-with? % "cm") #(let [x (Integer. (re-find #"^\d+" %))] (<= 150 x 193))))
(s/def ::height-in (s/and #(str/ends-with? % "in") #(let [x (Integer. (re-find #"^\d+" %))] (<= 59 x 76))))

(s/def ::byr #(<= 1920 (Integer. %) 2002))
(s/def ::iyr #(<= 2010 (Integer. %) 2020))
(s/def ::eyr #(<= 2020 (Integer. %) 2030))
(s/def ::hgt (s/or :cm ::height-cm :in ::height-in))
(s/def ::hcl (partial re-matches #"\#[a-f0-9]{6}"))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid (partial re-matches #"\d{9}"))

;; check how many passports meet the updated definition of valid
(prn :part-2 (->> input
                  (map (partial s/valid? ::passport))
                  (remove false?)
                  count))
