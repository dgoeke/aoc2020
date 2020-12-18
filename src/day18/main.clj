(ns aoc.day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

(def input (->> "day18-input.txt" io/resource slurp str/split-lines))

(def transform-options {:number read-string
                        :add    +
                        :mult   *})

(def parser-part1
  (insta/parser
   "<expr> = term | add | mult
    <term> = number | paren
    add = expr <'+'> term
    mult = expr <'*'> term
    <paren> = <space*> <'('> expr <')'> <space*>
    number = <space*> #'[0-9]+' <space*>
    space = ' '"))

(defn parse [parser input]
  (->> (parser input)
       (insta/transform transform-options)
       first))

(prn :part-1 (->> input (map (partial parse parser-part1)) (reduce +)))

(def parser-part2
  (insta/parser
   "<expr> = add | mult | number | paren
    mult = expr (<'*'> expr)
    add = (add|number|paren) (<'+'> (add|number|paren) )
    <paren> = <space*> <'('> expr <')'> <space*>
    number = <space*> #'[0-9]+' <space*>
    space = ' '"))

(prn :part-2 (->> input (map (partial parse parser-part2)) (reduce +)))
