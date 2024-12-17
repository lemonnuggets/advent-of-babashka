(ns aoc22.day01
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

; Question: https://adventofcode.com/2022/day/1

#?(:clj (def input
          (->> (slurp (io/resource "aoc22/day01.txt"))
               (str/split-lines)))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc22/day01.txt")
                         (str/split-lines)))))

(defn part-1
  "Run with (n)bb -x aoc22.day01/part-1"
  [_]
  (->> input
       prn))

(defn part-2
  "Run with (n)bb -x aoc22.day01/part-2"
  [_]
  (->> input
       prn))
