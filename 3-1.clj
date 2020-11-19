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


(defn partition- [n seq-]
    (partition n seq-)
    (if (>= (count seq-) n)
        (cons (take n seq-) (partition- n (drop n seq-)))
        '()
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

(partition 3 (range 20))
(range 20)

(defn if-test [a]
    (cond
        (> a 0) (println ">")
        (= a 0) (println "=")
        (< a 0) (println "<")
    )
)

(if-test -1)


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
