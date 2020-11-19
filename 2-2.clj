(ns labs.core
  (:gen-class))


(defn calc-trap [f a b]
    (let [res (* (/ (+ (f a) (f b)) 2) (- b a))]
        ;(println "calc-trap" "a:" a "b:" b "res:" res)
        res
    )
  )

;;--------------
;; best way
;;(let [seqq (map #(* 2 %) (range))]
;;    (->>
;;        (map
;;            #(calc-trap (fn [x] 1) %1 %2)
;;            seqq
;;            (rest seqq)
;;        )
;;        (reductions + 0)
;;        (take 10)
;;    )
;;)
;;--------------

;; #(+ % (calc-trap f % (+ % step)))
(defn get-integrator [f step]
    (let [ls (iterate
                (fn [[integr indexx]]
                    ;(println integr indexx)
                    (list
                        (+ integr (calc-trap f indexx (+ indexx step)))
                        (+ indexx step)
                    )
                )
                '(0 0))]
        (fn [k]
;;            (println "index" (/ k step))
            (first (nth ls (/ k step)))
        )
    )
)


(let [integrator (get-integrator (fn [x] x) 1)]
    (time (integrator 10000))  ;;"Elapsed time: 62.2315 msecs"
    (time (integrator 10015))  ;;"Elapsed time: 0.9351 msecs"
    (time (integrator  9998))  ;;"Elapsed time: 0.8551 msecs"
    (time (integrator     1))  ;;"Elapsed time: 0.0083 msecs"
    (time (integrator     3))  ;;"Elapsed time: 0.0426 msecs"
)
