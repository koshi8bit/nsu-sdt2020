(ns labs.core
  (:gen-class))



(defn heavy-inc [n]
    (Thread/sleep 100)
    (inc n)
)

(defn heavy-even? [n]
    (Thread/sleep 100)
    (even? n)
)



(defn pmap- [f seqq]
    (->>
        (map #(future (f %)) seqq)
        (doall)
        (map deref)
    )
)


(defn partition-old [n seq-]
    (partition n seq-)
    (if (>= (count seq-) n)
        (cons (take n seq-) (partition- n (drop n seq-)))
        '()
    )
)

(defn partition- [n seq-]

    (cond
        (> (count seq-) n)
            (do
                (println ">" seq-)
                (cons (take n seq-) (partition- n (drop n seq-)))
            )

        (= (count seq-) 0)
            (do
                (println "=" seq-)
                '()
            )

        (< 0 (count seq-) n)
            (do
                (println "<" seq-)
                (list seq-)
            )

    )
)

(defn filter- [pred seqq]
    ;;(println
        (->>
            (pmap- pred seqq)
            (map list seqq)
            (doall)
            (filter second)
            (map first)
        )
    ;;)
)

(partition- 3 (range 20))
;(range 20)
;(partition n seq-)

;;(defn tst [a]
;;    (if (> a 0) :big)
;;    (if (= a 0) :eq)
;;    (if (< a 0) :sm)
;;)

;;(tst 1)


;;(println "vanila filter")
;;(time
;;    (println (take 5 (filter heavy-even? (range 1000))))
;;)

;;(println)
;;(println "FAAAST filter")
;;(time
;;    (println (take 5 (filter- heavy-even? (range 1000))))
;;)
;;(println "fin!")
