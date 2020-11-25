(ns labs.core
  (:gen-class))

(defn heavy-even? [n]
    (Thread/sleep 100)
    (even? n)
)


(defn pmap-my [f coll]
    (->>
        (map #(future (doall (filter f %))) coll)
        (doall)
        (map deref)
    )
)


(defn partition-my [n coll]
    ;(println "partition-" n seq-)
    (let [c (count coll)]
        (cond
            (> c n)
                (cons (take n coll) (partition-my n (drop n coll)))

            (= c 0)
                '()

            (and (< 0 c) (<= c n))
                (list coll)
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
        (partition-my res coll)
    )
)
;;(println (split-by-threads 4 (range 40)))
;;(println "Math/round" (Math/round 2.6))


(defn filter-my [threads pred coll]
    (->>
        (split-by-threads threads coll)
        (pmap-my pred)
        (doall)
        (apply concat)
    )
)





(defn heavy-count [coll]
    (Thread/sleep 100)
    (count coll)
)

(let [f-pred (fn [coll] (>= (heavy-count coll) 5))
      coll (list (range 2) (range 5) (range 7) (range 10))]
    (println "vanila filter begin")
    (time
        (println (filter f-pred coll))
    )
    (println "vanila filter end")

    (println)
    (println "FAAAST filter begin")
    (time
        (println (filter-my 4 f-pred coll))
    )
    (println "FAAAST filter end")
    (println "fin!")
)

