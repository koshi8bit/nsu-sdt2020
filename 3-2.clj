(ns labs.core
  (:gen-class))

(defn heavy-even? [n]
    (Thread/sleep 100)
    (even? n)
)


(defn pmap-my [f coll]
    (->>
        (map #(future (f %)) coll)
        (doall)
        (map deref)
    )
)


(defn partition-my [n coll]
    ;;(println "partition-my" n coll)
    (when (not (empty? coll))
        (lazy-seq
            (cons
                (take n coll)
                (partition-my n (drop n coll))
            )
        )
    )
)
;;(let [r (range 10) split 4]
;;    (println (take 5 (partition-my split r)))
;;    (println (take 5 (partition split r)))
;;)


;; chunkk - данных на кусочке (последовательно)
;; batch  - одновременно несколько задач (параллельно)
(defn filter-my [chunkk batch f-pred coll]
    (->>
        coll
        (partition-my chunkk)
        (partition-my batch)
        (mapcat (fn [coll2] (pmap-my #(doall (filter f-pred %)) coll2)))
        (apply concat)
    )
)


(defn heavy-count [coll]
    (Thread/sleep 100)
    (count coll)
)

(let [f-pred (fn [coll] (>= (heavy-count coll) 3))
      coll (list (range 1) (range 2) (range 3) (range 4))]
;;(let [f-pred even? coll (range 20)]
    (println "vanila filter begin")
    (time
        (println (filter f-pred coll))
    )
    (println "vanila filter end")

    (println)
    (println "FAAAST filter begin")
    (time
        (println "result" (filter-my 3 2 f-pred coll))
    )
    (println "FAAAST filter end")
    (println "fin!")

    ;;(pmap-my #(map inc %) '((1 2 3 4) (5) (6 7))) ;; (false true false true false true false)

)

