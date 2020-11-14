(ns lab1.core
  (:gen-class))

(defn joiner [w alph]
    (println "joiner" w alph)
    (map
        #(cons % w)
        (filter #(not= (first w) %) alph)
    )
)
;;(println (joiner `() `(:a :b :c)))
;;(println (joiner `(:a) `(:a :b :c)))


(defn joiner-words [w alph]
    (println "joiner-words" w alph)
    (reduce concat
        (map #(joiner % alph) w)
    )
)
;;(println (joiner-words '((:a) (:b)) '(:a :b :c)))


(defn mix [n alph]
    (nth (iterate #(joiner-words % alph) '(())) n)
)
(println (mix 3 `(:a [:b :c] "d")))
