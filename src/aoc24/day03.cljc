(ns aoc24.day03
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            #?(:cljs [promesa.core :as p])
            [clojure.string :as s]))

; Question: https://adventofcode.com/2024/day/3

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day03.txt"))))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day03.txt")))))

(defn part-1
  "Run with (n)bb -x aoc24.day03/part-1"
  [_]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ a b]] (* (parse-long a) (parse-long b))))
       (apply +)
       prn))

(defn part-2
  "Run with (n)bb -x aoc24.day03/part-2"
  [_]
  (-> input
      (s/split #"(?s)don't\(\).*?do\(\)")
      (->> (apply str))
      (s/replace #"don't\(\).*$" "")
      (->> (re-seq #"mul\((\d+),(\d+)\)")
           (map (fn [[_ a b]] (* (parse-long a) (parse-long b))))
           (apply +))
      prn))
