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


;; chunkk - данных на кусочке
;; batch  - одновременно несколько задач
(defn filter-my [chunkk batch pred coll]
    (->>
        ;;(split-by-threads threads coll)
        (partition-my (* chunkk batch) coll)
        (#(do (println %) %))
        (map #(partition-my chunkk %))
        (#(do (println %) %))
        ;;(map (pmap-my #(doall (filter pred %))))
;;        (apply concat)
;;        (pmap-my #(doall (filter pred %)))
;;        (doall)
;;        (apply concat)
    )
)


(defn heavy-count [coll]
    (Thread/sleep 100)
    (count coll)
)

;;(let [f-pred (fn [coll] (>= (heavy-count coll) 4))
;;      coll (list (range 1) (range 2) (range 3) (range 4))]
(let [f-pred even? coll (range 20)]
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
)

