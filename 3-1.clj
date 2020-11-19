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



(defn pmap- [f coll]
    (->>
        ;(map #(future (f %)) coll)
        (map #(future (doall (filter f %))) coll)
        (doall)
        (map deref)
    )
)




(defn partition- [n seq-]
    ;(println "partition-" n seq-)
    (let [c (count seq-)]
        (cond
            (> c n)
                (cons (take n seq-) (partition- n (drop n seq-)))

            (= c 0)
                '()

            (and (< 0 c) (<= c n))
                (list seq-)
        )
    )
)


;;(let [r (range 5) split 1]
;;    (println (partition- split r))
;;    ;(println (partition split r))
;;)

(defn split-by-threads [threads coll]
    (let [res (Math/ceil (/ (count coll) threads))] ;; Math/ceil vs Math/round
        ;(println "split-by-threads t:" threads "res:" res "coll:" coll)
        (partition- res coll)
    )
)
;;(println "Math/round" (Math/round 2.6))



;;(let [coll (range 40) t 4]
;;    (println (split-by-threads t coll))
;;)



(defn filter- [threads pred coll]
    ;;(println
        (->>
            (split-by-threads threads coll)
            (pmap- pred)
            (doall)
            (map conj)
        )
    ;;)
)




(let [f-pred heavy-even? coll (range 40)]
    (println "vanila filter <")
    (time
        (println (filter f-pred coll))
    )
    (println "vanila filter >")

    (println)
    (println "FAAAST filter <")
    (time
        (println (filter- 4 f-pred coll))
    )
    (println "FAAAST filter >")
    (println "fin!")
)

