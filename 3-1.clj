(ns labs.core
  (:gen-class))

(defn pmap- [f seqq]
    (->>
        (map #(future (f %)) seqq)
        (doall)
        (map deref)
    )
)

(defn heavy-inc [n]
    (Thread/sleep 100)
    (inc n)
)

(defn heavy-even? [n]
    (Thread/sleep 100)
    (even? n)
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

(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "vanila filter")
    (time
        (println (take 5 (filter heavy-even? (range 1000))))
    )

    (println)
    (println "FAAAST filter")
    (time
        (println (take 5 (filter- heavy-even? (range 1000))))
    )

    (println)
    (println "vanila filter")
    (time
        (println (take 5 (filter heavy-even? (range 1000))))
    )

    (println "fin!")
)


(-main)
