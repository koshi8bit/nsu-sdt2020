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

    (cond
        (> (count seq-) n)
            (cons (take n seq-) (partition- n (drop n seq-)))

        (= (count seq-) 0)
            '()

        (< 0 (count seq-) n)
            (list seq-)
    )
)


;;(let [r (range 5) split 3]
;;    (println (partition- split r))
;;    (println (partition split r))
;;)

(defn split-by-threads [threads coll]
    (let [res (Math/ceil (/ (count coll) threads))] ;; Math/ceil vs Math/round
        ;(println "split-by-threads t:" threads "res:" res "coll:" coll)
        res
    )
)
(println "Math/round" (Math/round 2.6))



(let [coll (range 5) t 4]
    (println (split-by-threads t coll))
    ;(println (partition- (split-by-threads t coll) coll))
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
