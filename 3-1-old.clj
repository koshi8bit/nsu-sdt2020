(ns lab1.core
  (:gen-class))

;;(let [foo (future (Thread/sleep 1000) 3)]
;;    @foo
;;)

(defn heavy-inc [n]
    (Thread/sleep 100)
    (inc n)
)

;;(time
;;    (->>
;;        (iterate inc 0)
;;        (take 10)
;;        (map #(future (heavy-inc %)))
;;        (map deref)
;;        (doall)
;;    )
;;)

(defn partition-rec [n seq-]
    (partition n seq-)
    (if (>= (count seq-) n)
        (cons (take n seq-) (partition-rec n (drop n seq-)))
        '()
    )
)

(defn partition- [n seq-]
    (lazy-seq
        (cons (take n seq-) (drop n seq-))
    )
)

(defn filterrr [pred seqq threads]
    (let [l (Math/ceil (/ (count seqq) threads))]
        (println "l" l)
    )
)


;;(partition-rec 2 '(1 2 3 4 5 6))

;;(filterrr even? '(1 2 3 4 5 6 2)


(defn pmap- [f seqq]
    (->>
        (map #(future (f %)) seqq)
        (doall)
        (map deref)
    )
)

(time
    (->>
        (pmap- heavy-inc (range 200))
        (doall)
        (take 10)
    )
)

(->>
    (range 100)
    (map list
    (pmap- even?)
    (doall)
    (take 10)

)

;;(let [r (range 100)]
;;    (list r (map inc r)))



;;----------------------

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
    (Thread/sleep 1000)
    (inc n)
)

(defn filter- [pred seqq]
    (println
        (doall
            (map list seqq (pmap- pred seqq))
         )
    )
)

(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!")

;;    (let [r (range 10)]
;;      (println (doall (map list r (pmap- heavy-inc r))))
;;    )

;;    (println (->>
;;        (range 100)
;;        (pmap- even?)
;;        (doall)
;;        (take 10)
;;    ))

    (filter- even? (range 10))

    (println "fin2!")
)







;;(time
;;    (->>
;;        (pmap- heavy-inc (range 15))
;;        (doall)
;;        (take 10)
;;    )
;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
            (map list seqq (pmap- pred seqq))
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



