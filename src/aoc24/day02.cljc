(ns aoc24.day02
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

; Question: https://adventofcode.com/2024/day/2

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day02.txt"))
               (str/split-lines)))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day02.txt")
                         (str/split-lines)))))

(defn- steady?
  [report]
  (let [diffs (map (fn [[a b]] (- a b)) (partition 2 1 report))]
    (and (or (every? pos-int? diffs) (every? neg-int? diffs))
         (every? #(<= (abs %) 3) diffs))))
(comment
  (steady? [-1 -2 -1 -2 -1]))

(defn- safe?
  [report]
  (-> report
      (str/split #" ")
      (->> (map parse-long)
           steady?)))
(comment
  (safe? "7 6 4 2 1")
  (safe? "1 2 7 8 9"))

(defn part-1
  "Run with (n)bb -x aoc24.day02/part-1"
  [_]
  (->> input
       (filter safe?)
       count
       prn))

(defn- remove-at
  [coll idx]
  (concat (subvec coll 0 idx) (subvec coll (inc idx))))

(defn- one-less-combinations
  [reports]
  (map (partial remove-at reports) (range (count reports))))
(comment
  (one-less-combinations [1 2 3 4 5 6]))

(defn- mostly-safe?
  [report]
  (-> report
      (str/split #" ")
      (->> (map parse-long)
           vec
           one-less-combinations
           (some steady?))))

(defn part-2
  "Run with (n)bb -x aoc24.day02/part-2"
  [_]
  (->> input
       (filter mostly-safe?)
       count
       prn))
