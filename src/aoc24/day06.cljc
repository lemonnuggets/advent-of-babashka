(ns aoc24.day06
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [await slurp]])
            #?(:cljs [promesa.core :as p])
            [clojure.string :as s]
            [utils.utils :as u]))

; Question: https://adventofcode.com/2024/day/6

#?(:clj (def input
          (->> (slurp (io/resource "aoc24/day06.txt"))
               (s/split-lines)))
   :cljs (def input
           (await (p/->> (slurp "resources/aoc24/day06.txt")
                         (s/split-lines)))))

(defn- guard-facing [m] #_(u/print m) (some (partial re-find #"\^|>|v|<") m))
(comment
  (guard-facing ["....#....." ".........#" ".........." "..#......."
                 ".......#.." ".........." ".#..^....." "........#."
                 "#........." "......#..."]))

(defn- guard-col
  [m guard]
  (->> (map #(s/index-of % guard) m)
       (filter identity)
       first))

(defn- guard-row
  [m guard]
  (->> (map-indexed (fn [idx s] (when (s/index-of s guard) idx)) m)
       (filter identity)
       first))

(defn- last-obstacle-row
  [m col end-row]
  (or (s/last-index-of (->> (map #(get % col) m)
                            (take end-row)
                            (s/join ""))
                       "#")
      ##-Inf))

(defn- first-obstacle-row
  [m col start-row]
  (+ (inc start-row)
     (or (s/index-of (->> (map #(get % col) m)
                          (drop (inc start-row))
                          (s/join ""))
                     "#")
         ##Inf)))

(defn- walk>
  [s]
  (let [obstacle-path (map first (re-seq #">(\.|X)*#" s))
        path          (first (first (re-seq #">(\.|X)*" s)))
        obstacled-replacement (s/join ""
                                      (-> (dec (count path))
                                          (repeat \X)
                                          (conj \v)
                                          reverse))
        replacement   (s/join ""
                              (-> (count path)
                                  (repeat \X)))]
    (if (empty? obstacle-path)
      (s/replace-first s path replacement)
      (s/replace-first s path obstacled-replacement))))

(defn- walk<
  [s]
  (let [obstacle-path (map first (re-seq #"#(\.|X)*<" s))
        path          (first (first (re-seq #"(\.|X)*<" s)))
        obstacled-replacement (s/join ""
                                      (-> (dec (count path))
                                          (repeat \X)
                                          (conj \^)))
        replacement   (s/join ""
                              (-> (count path)
                                  (repeat \X)))]
    (if (empty? obstacle-path)
      (s/replace-first s path replacement)
      (s/replace-first s path obstacled-replacement))))

(defmulti walk guard-facing)

(defmethod walk "^"
  [m]
  (let [g-row        (guard-row m "^")
        g-col        (guard-col m "^")
        obstacle-row (last-obstacle-row m g-col g-row)]
    (walk (vec (map-indexed
                (fn [idx s]
                  (cond (= (inc obstacle-row) idx) (u/replace-at s ">" g-col)
                        (and (> idx (inc obstacle-row)) (<= idx g-row))
                        (u/replace-at s "X" g-col)
                        :else s))
                m)))))

(defmethod walk ">"
  [m]
  (let [row-idx (guard-row m ">")]
    (walk (assoc m row-idx (walk> (get m row-idx))))))

(defmethod walk "v"
  [m]
  (let [g-row        (guard-row m "v")
        g-col        (guard-col m "v")
        obstacle-row (first-obstacle-row m g-col g-row)]
    (walk (vec (map-indexed (fn [idx s]
                              (cond (= (dec obstacle-row) idx)
                                    (u/replace-at s "<" g-col)
                                    (and (<= g-row idx) (> obstacle-row idx))
                                    (u/replace-at s "X" g-col)
                                    :else s))
                            m)))))

(defmethod walk "<"
  [m]
  (let [row-idx (guard-row m "<")]
    (walk (assoc m row-idx (walk< (get m row-idx))))))

(defmethod walk :default [m] m)

(defn- x-count [m] (apply + (map (comp count (partial re-seq #"X")) m)))

(defn part-1
  "Run with (n)bb -x aoc24.day06/part-1"
  [_]
  (->> input
       walk
       x-count
       prn))

(defn- guard-dir
  [m]
  (case (guard-facing m)
    "^" [-1 0]
    ">" [0 1]
    "v" [1 0]
    "<" [0 -1]
    (throw (ex-info "Guard facing invalid" {}))))

(defn- rot90 [[dir-r dir-c]] [dir-c (- dir-r)])

(defn- guard-pos
  [m]
  (let [g (guard-facing m)] [(guard-row m g) (guard-col m g)]))

(defn- next-pos [[dir-r dir-c] [pos-r pos-c]] [(+ dir-r pos-r) (+ dir-c pos-c)])

(defn- char-at
  [m [row col]]
  (-> m
      (get row)
      (get col)))

(defn- in-bounds?
  [m [row col]]
  (and (<= 0 row) (> (count m) row) (<= 0 col) (> (count (first m)) col)))

(defn- obstacle? [m pos] (= \# (char-at m pos)))

(defn- step
  [m [dir pos]]
  (let [new-pos (next-pos dir pos)]
    (when (in-bounds? m new-pos)
      (if (obstacle? m new-pos) [(rot90 dir) pos] [dir new-pos]))))

(defn- loops?
  ([m] (loops? m [(guard-dir m) (guard-pos m)] #{}))
  ([m curr seen]
   (boolean
    (when-let [next (step m curr)]
      (if (contains? seen next) true (loops? m next (conj seen next)))))))
(comment
  (loops? ["....#....." ".........#" ".........." "..#......." ".......#.."
           ".........." ".#..^....." "........#." "#........." "......#..."])
  (loops? ["....#....." ".........#" ".........." "..#......." ".......#.."
           "....#....." ".#..^..#.." "........#." "#........." "....#.##.."]))

(defn- clear? [m pos] (= \. (char-at m pos)))

(defn- add-obstacle [m pos] (when (clear? m pos) (u/replace-at m "#" pos)))

(comment
  (let [m    ["....#....." ".........#" ".........." "..#......." ".......#.."
              ".........." ".#..^....." "........#." "#........." "......#..."]
        rows (count m)
        cols (count (first m))]
    #_(map (partial u/replace-at))
    (->> (u/cartesian-product (range rows) (range cols))
         (keep (partial add-obstacle m)))))

(defn- looping-obstacles
  [m]
  (let [rows (count m)
        cols (count (first m))]
    ;TODO: use positions of Xs generated by (walk m), as only obstacles
    ;placed along this path will affect the path taken by the guard.
    ;Reduces search space from 16,900 -> 4,973 obstacle positions
    (->> (u/cartesian-product (range rows) (range cols))
         (keep (partial add-obstacle m))
         (filter loops?))))
(comment
  (map u/print
       (looping-obstacles ["....#....." ".........#" ".........." "..#......."
                           ".......#.." ".........." ".#..^....." "........#."
                           "#........." "......#..."])))

(defn part-2
  "Run with (n)bb -x aoc24.day06/part-2"
  [_]
  (->> input
       looping-obstacles
       count
       prn))
