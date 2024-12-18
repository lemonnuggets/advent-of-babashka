(ns aoc24.day07
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as s]
            #?(:cljs [promesa.core :as p])
            [utils.utils :as u]))

; Question: https://adventofcode.com/2024/day/7

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day07.txt"))
               (s/split-lines)))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day07.txt")
                         (s/split-lines)))))

(def ^:private operator-combinations
  (memoize (fn [operators n]
             (case n
               0 ()
               1 (map list operators)
               (->> (operator-combinations operators (dec n))
                    (u/cartesian-product (operator-combinations operators 1))
                    (map flatten))))))
(comment
  (operator-combinations operators-1 5)
  (u/cartesian-product ["+" "x"] ["+" "x"]))

(defn- interleave-operators
  [numbers operators]
  (-> operators
      (interleave (rest numbers))
      (conj (first numbers))))

(defn- eval-equation
  [equation]
  (if (= 1 (count equation))
    (first equation)
    (let [[op1 op op2 & rest] equation]
      (eval-equation (conj rest (op op1 op2))))))
(comment
  (let [[a b c & d] [1 2 3 4]] [a b c d])
  (conj nil 1)
  (eval-equation '(1 * 2 + 3)))

(defn- solution
  [operators equation]
  (let [[test-val numbers] (s/split equation #": ")
        test-val (parse-long test-val)
        numbers  (map parse-long (s/split numbers #" "))]
    (->> (dec (count numbers))
         (operator-combinations operators)
         (map (partial interleave-operators numbers))
         (filter #(= (eval-equation %) test-val)))))

(defn- solution-exists?
  [operators equation]
  (when-not (empty? (solution operators equation))
    (-> equation
        (s/split #": ")
        first
        parse-long)))

(defn- solution-vals-sum
  [operators input]
  (->> input
       (keep (partial solution-exists? operators))
       (apply +)))

(comment
  (solution-vals-sum operators-2
                     ["190: 10 19" "3267: 81 40 27" "83: 17 5" "156: 15 6"
                      "7290: 6 8 6 15" "161011: 16 10 13" "192: 17 8 14"
                      "21037: 9 7 18 13" "292: 11 6 16 20"]))

(def ^:private operators-1 [+ *])

(defn part-1
  "Run with (n)bb -x aoc24.day07/part-1"
  [_]
  (->> input
       (solution-vals-sum operators-1)
       prn))

(defn- ||
  [& args]
  (->> args
       (map str)
       s/join
       parse-long))

(def ^:private operators-2 [+ * ||])

(defn part-2
  "Run with (n)bb -x aoc24.day07/part-2"
  [_]
  (->> input
       (solution-vals-sum operators-2)
       prn))
(comment
  (part-2 "a"))
