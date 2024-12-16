(ns aoc24.day04
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [await slurp]])
            #?(:cljs [promesa.core :as p])
            [clojure.string :as s]))

; Question: https://adventofcode.com/2024/day/4

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day04.txt"))))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day04.txt")))))

(defn- strip-first
  [m nrows]
  (if (> (count m) nrows)
    (vec (concat (map (comp (partial apply str) #(or (rest %) [""]))
                      (subvec m 0 nrows))
                 (subvec m nrows)))
    (map (comp (partial apply str) #(or (rest %) [""])) m)))

(defn- get-first
  [m nrows]
  (apply str
         (map #(or (first %) "")
              (if (> (count m) nrows) (subvec m 0 nrows) m))))

(defn- diagonals*
  [m diags]
  (if (every? empty? m)
    diags
    (let [nextm (strip-first m (inc (count diags)))
          diag  (get-first m (inc (count diags)))]
      (diagonals* nextm (conj diags diag)))))

(defn diagonals [m] (diagonals* (vec m) []))

(defn rot90
  [m]
  (let [n (max (count m) (count (first m)))]
    (map (fn [i] (apply str (map #(get % i) m))) (range n))))
(comment
  (rot90 ["abcd" "efgh"]))

(defn xmas-count
  [m]
  (->> (concat m (diagonals m) (rot90 m) (diagonals (rot90 (reverse m))))
       (mapcat (partial re-seq #"(?=(XMAS)|(SAMX))"))
       count))

(defn part-1
  "Run with (n)bb -x aoc24.day04/part-1"
  [_]
  (->> input
       s/split-lines
       xmas-count
       prn))

(defn- partition3x3
  [m]
  (->> (partition 3 1 m)
       (mapcat #(->> %
                     (apply interleave)
                     (partition 9 3)
                     (map (partial partition 3))))))

(defn- x-mas?
  [m]
  (let [[[a _ b] [_ c _] [d _ e]] m
        diags [[a c e] [b c d]]]
    (every? #(or (= [\M \A \S] %) (= [\S \A \M] %)) diags)))

(defn x-mas-count
  [m]
  (->> (partition3x3 m)
       (filter x-mas?)
       count))

(defn part-2
  "Run with (n)bb -x aoc24.day02/part-2"
  [_]
  (->> input
       s/split-lines
       x-mas-count
       prn))
