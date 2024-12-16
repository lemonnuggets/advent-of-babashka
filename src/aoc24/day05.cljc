(ns aoc24.day05
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [await slurp]])
            #?(:cljs [promesa.core :as p])
            [clojure.set :as set]
            [clojure.string :as s]))

; Question: https://adventofcode.com/2024/day/5

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day05.txt"))
               (s/split-lines)))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day05.txt")
                         (s/split-lines)))))

(defn- add-rule
  [book rule]
  (let [[a b] (s/split rule #"\|")] (assoc book a (conj (get book a #{}) b))))

(defn- rule-book [rules] (reduce add-rule {} rules))

(defn- mismatched-pages
  [book update]
  (reduce (fn [seen-pages page]
            (let [mismatched-page (set/intersection seen-pages
                                                    (get book page #{}))]
              (if (= #{} mismatched-page)
                (conj seen-pages page)
                (reduced [page (first mismatched-page)]))))
          #{}
          update))

(defn- correctly-ordered?
  [book update]
  (->> (mismatched-pages book update)
       vector?
       not))

(defn- middle [coll] (nth coll (quot (count coll) 2)))

(defn part-1
  "Run with (n)bb -x aoc24.day05/part-1"
  [_]
  (let [[rules [_ & updates]] (split-with not-empty input)
        updates (map #(s/split % #",") updates)
        book    (rule-book rules)]
    (->> (filter (partial correctly-ordered? book) updates)
         (map (comp parse-long middle))
         (apply +)
         prn)))

(defn- insert* [page [part-a part-b]] (flatten [part-a page part-b]))
(defn- insert
  [book update page]
  (->> (range (inc (count update)))
       (map #(split-at % update))
       (some (fn [split-update]
               (when (->> split-update
                          (insert* page)
                          (correctly-ordered? book))
                 split-update)))
       (insert* page)))

(defn- merge
  [book update-a update-b]
  (cond (empty? update-a) update-b
        (empty? update-b) update-a
        :else             (let [[page & rest] update-b]
                            (merge book (insert book update-a page) rest))))

(defn- halve [coll] (split-at (quot (count coll) 2) coll))

(defn- reorder
  [book update]
  (cond (>= 1 (count update)) update
        (correctly-ordered? book update) update
        (= 2 (count update))  (reverse update)
        :else                 (let [[half-a half-b] (halve update)]
                                (merge book
                                       (reorder book half-a)
                                       (reorder book half-b)))))

(defn part-2
  "Run with (n)bb -x aoc24.day05/part-2"
  [_]
  (let [[rules [_ & updates]] (split-with not-empty input)
        updates (map #(s/split % #",") updates)
        book    (rule-book rules)]
    (->> (filter (comp not (partial correctly-ordered? book)) updates)
         (map (comp parse-long middle (partial reorder book)))
         (apply +)
         prn)))
