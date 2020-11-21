(ns lab1.core
  (:gen-class))

(defn joiner [w alph]
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

(defn joiner-words [w alph]
    (loop [acc `() w w alph alph]
        ;(println "f2" w alph)
        (if (empty? w)
            acc
            (recur
                (concat
                    acc
                    (joiner (first w) alph)
                )
                (rest w)
                alph
            )
        )
    )
)



(defn mix [n alph]
    ;(println "f3+" n alph)
    (loop [acc '(()) n n alph alph]
        (if (> n 0)
            (recur
                (joiner-words acc alph)
                (dec n)
                alph
            )
            acc
        )
    )
)



;(println "f1 a" (f1 '() '(:a :b :c)))      ;((a) (b) (c))
;(println "f1 b" (f1 '(:a) '(:a :b :c)))    ;((b a) (c a))

;(println "f2" (f2 '((:a) (:b)) '(:a :b :c)))   ;((b a) (c a) (a b) (c b))
;(println "f2+" (f2 '(()) '(:a :b :c)))

;(println "f3 a" (f3 2 '(:a :b :c)))
(println "f3 b" (mix 2 '(:a [:b :c] "d")))
