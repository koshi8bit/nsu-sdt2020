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
(defn get-integrator [f begin step]
    (let [ls (iterate
                (fn [[integr indexx]]
                    ;(println integr indexx)
                    (list
                        (+ integr (calc-trap f indexx (+ indexx step)))
                        (+ indexx step)
                    )
                )
                (list begin 0))]
        (fn [k]
;;            (println "index" (/ k step))
            (first (nth ls (/ k step)))
        )
    )
)


(let [integrator (get-integrator (fn [x] (* x x)) 0 1)]
    (time (integrator 10000))  ;; "Elapsed time: 62.2315 msecs"
    (time (integrator 10015))  ;; "Elapsed time: 0.9351 msecs"
    (time (integrator  9998))  ;; "Elapsed time: 0.8551 msecs"
)

(let [integrator (get-integrator (fn [x] (* x x)) 0 1/100)]
    (println (integrator  3))  ;; 180001/20000 ~ 9
    (println (integrator  2))  ;; 26667/10000  ~ 2.66667
)
