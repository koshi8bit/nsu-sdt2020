(ns labs.core
  (:gen-class))


(defn calc-trap [f a b]
    (let [res (* (/ (+ (f a) (f b)) 2) (- b a))]
        ;(println "calc-trap" "a:" a "b:" b "res:" res)
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

;; #(+ % (calc-trap f % (+ % step)))
(defn get-integrator [f step]
    (let [ls (iterate
                (fn [lst]
                    (println lst)
                    (list
                        (/ (first lst) 10)
                        (+ (second lst) step)
                    )
                )
                '(1 0))]
        (fn [k]
;;            (println "index" (/ k step))
;;            (println (take 100 ls))
            (nth ls (/ k step))
        )
    )
)

;; DELETE ME !!!!!!!!!!!!!!!!----1-1-1-1-1-1-1
;;(defn create-seq [f a step]
;;    (map
;;        first
;;        (iterate
;;            (fn [sum x] (+ sum (* (* (+ (f x) (f (+ x step))) 0.5) step)) (+ x step))
;;            [0.0 a]
;;        )
;;    )
;;)

(let [integrator (get-integrator (fn [x] x) 1)]
    ;(time (integrator 10000))  ;; Elapsed time: 332.0941 msecs
    ;(time (integrator 10015))  ;; Elapsed time: 1.2308 msecs
    (time (integrator     5))  ;; Elapsed time: 0.5012 msecs
    (time (integrator     1))  ;; 1/2 для y=x
    (time (integrator     2))  ;; 3/2 для y=x ???
    (println (integrator     5))
)
