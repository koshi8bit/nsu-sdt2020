(ns labs.core
  (:gen-class))


(defn calc-trap [f a b]
  (* (/ (+ (f a) (f b)) 2) (- b a)))

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
    (let [ls (iterate #(+ % (calc-trap f % (+ % step)))
                      begin)]
        (fn [k]
            (nth ls (/ k step))
        )
    )
)

(let [integrator (get-integrator (fn [x] x) 0 1/2)]
    ;(time (integrator 10000))  ;; Elapsed time: 332.0941 msecs
    ;(time (integrator 10015))  ;; Elapsed time: 1.2308 msecs
    ;(time (integrator  9985))  ;; Elapsed time: 0.5012 msecs
    (time (integrator     1))  ;; 1/2 для y=x
    (time (integrator     2))  ;; 3/2 для y=x ???
)

