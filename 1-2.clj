(ns lab1.core
  (:gen-class))

(defn f1 [w alph]
    ;(println "f1" w alph)
    (loop [acc `() w w alph alph]
        (if (empty? alph)
            acc
            (if (not= (first w) (first alph))
                (recur (cons (cons (first alph) w) acc)
                        w (rest alph))
                (recur acc w (rest alph))
            )
        )
    )
)

(defn f2 [w alph]
    (loop [acc `() w w alph alph]
        ;(println "f2" w alph)
        (if (empty? w)
            acc
            (recur
                (concat
                    acc
                    (f1 (first w) alph)
                )
                (rest w)
            )
        )
    )
)


(defn f3 [n alph]
    ;(println "f3+" n alph)
    (if (> n 1)
        (f2
            (f3 (dec n) alph)
            alph
        )
        (f2 '(()) alph))
)


;(println "f1 a" (f1 '() '(:a :b :c)))      ;((a) (b) (c))
;(println "f1 b" (f1 '(:a) '(:a :b :c)))    ;((b a) (c a))

;(println "f2" (f2 '((:a) (:b)) '(:a :b :c)))   ;((b a) (c a) (a b) (c b))
;(println "f2+" (f2 '(()) '(:a :b :c)))

(println "f3 a" (f3 2 '(:a :b :c)))
(println "f3 b" (f3 2 '(:a [:b :c] "d")))





(defn single_connector [word lst]
  (loop [acc `() word word lst lst]
    (if (empty? lst)
      acc
      (if (= (first word) (first lst))
        (recur acc word (rest lst))
        (recur (cons (cons (first lst) word) acc)
               word (rest lst))))))
