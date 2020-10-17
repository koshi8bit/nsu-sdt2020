(ns lab1.core
  (:gen-class))

(defn calc-trap [f a b]
    ;(println "calc-trap" a b)
  (* (/ (+ (f a) (f b)) 2) (- b a)))

(defn integrate [f end delta]
  (println "integrate" end)
  (if (<= end 0)
    0
    (let [prevv (- end delta)]
            ;(println "integrate prevv" prevv)
      (+
       (integrate f prevv delta)
       (calc-trap f prevv end)))))

(let [maxx 10 delta 2]

    (take (/ maxx delta)
        (iterate #(+ % delta) 0)
    )
)

(comment
(let [seqq (map #(* 2 %) (range))]
    (take 10
        (reductions + 0
            (map
                #(calc-trap (fn [x] (* x x)) %1 %2)
                seqq
                (rest seqq)
            )
        )
    )
)
;(->>
;    (take 10 (map #(* 2 %) (range)))
;    (map #(calc-trap (fn [x] (* x x)) %1 %2)
;)
;(take 10 (seq (range 0 4)))
)



(let [seqq (map #(* 2 %) (range))]

    (def foo
        (lazy-seq
            (cons 1
                (map
                    #(calc-trap (fn [x] (* x x)) %1 %2)
                    seqq
                    (rest seqq)
                )
            )
        )
    )

    (println "result" (take 3 (rest foo)))

;    (->>
;        (map
;            #(calc-trap (fn [x] (* x x)) %1 %2)
;            seqq
;            (rest seqq)
;        )
;        (reductions + 0)
;        (take 10)
;    )


)

