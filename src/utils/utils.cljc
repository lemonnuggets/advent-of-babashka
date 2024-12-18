(ns utils.utils)

(defn print [coll] (prn) (dorun (map prn coll)))

(defn replace-at
  [s replacement idx]
  (if (vector? s)
    (let [[row col] idx] (assoc s row (replace-at (nth s row) replacement col)))
    (str (subs s 0 idx) replacement (subs s (inc idx)))))
(comment
  (replace-at "hello" "b" 2)
  (replace-at ["hello" "bello" "mello" "jello"] "x" [2 0]))

(defn cartesian-product
  [coll-a coll-b]
  (mapcat (fn [a] (map (fn [b] [a b]) coll-b)) coll-a))
(comment
  (cartesian-product [1 2 3] [4 5 6]))
