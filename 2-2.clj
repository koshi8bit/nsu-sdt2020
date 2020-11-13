(ns labs.core
  (:gen-class))


(defn calc-trap [f a b]
    (let [res (* (/ (+ (f a) (f b)) 2) (- b a))]
        (println "calc-trap" res)
        res
    )
  )

;;--------------
;; better way
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

(defn get-integrator [f begin step]
    (let [ls (iterate
                #(+ % (calc-trap f % (+ % step)))
                begin)]
        (fn [k]
;;            (println "index" (/ k step))
;;            (println (take 100 ls))
            (nth ls (/ k step))
        )
    )
)

(let [integrator (get-integrator (fn [x] x) 0 1)]
    ;(time (integrator 10000))  ;; Elapsed time: 332.0941 msecs
    ;(time (integrator 10015))  ;; Elapsed time: 1.2308 msecs
    (time (integrator     20))  ;; Elapsed time: 0.5012 msecs
    (time (integrator     1))  ;; 1/2 для y=x
    ;(time (integrator     2))  ;; 3/2 для y=x ???
)

