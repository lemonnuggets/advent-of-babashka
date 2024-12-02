(ns aoc24.day01
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

; Question: https://adventofcode.com/2024/day/1

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day01.txt"))
               (str/split-lines)))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day01.txt")
                         (str/split-lines)))))

(defn part-1
  "Run with (n)bb -x aoc24.day01/part-1"
  [_]
  (->> input
       (map #(str/split % #"   "))
       ((juxt (comp sort (partial map (comp parse-long first)))
              (comp sort (partial map (comp parse-long second)))))
       (apply (partial map (comp abs -)))
       (apply +)
       prn))

(defn part-2
  "Run with (n)bb -x aoc24.day02/part-2"
  [_]
  (let [[ids counts] (->> input
                          (map #(str/split % #"   "))
                          ((juxt (comp (partial map (comp parse-long first)))
                                 (comp frequencies
                                       (partial map
                                                (comp parse-long second))))))]
    (->> ids
         (map #(* % (or (counts %) 0)))
         (apply +)
         prn)))
