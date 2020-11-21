(ns lab1.core
  (:gen-class))

(defn my-map [f data]
  (reduce (fn [x y] (conj x (f y)))
          []
          data))

(defn my-filter [f data]
  (reduce #(if (f %2)
               (conj %1 %2)
               %1)
          []
          data))

(println (my-map #(* %1 %1) '(2 4 3)))
(println (my-filter even? (range 1 10)))

