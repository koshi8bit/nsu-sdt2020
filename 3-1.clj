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
