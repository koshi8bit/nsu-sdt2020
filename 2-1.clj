(ns labs.core
  (:gen-class))

(defn calc-trap [f a b]
    ;(println "calc-trap" a b)
  (* (/ (+ (f a) (f b)) 2) (- b a)))


(declare integrate-mem)

(defn integrate [f end delta]
  (println "integrate" end)
  (if (<= end 0)
    0
    (let [prevv (- end delta)]
            ;(println "integrate prevv" prevv)
      (+
       (integrate-mem f prevv delta)
       (calc-trap f prevv end)))))

(def integrate-mem (memoize integrate))

(letfn [(f [x] (* x x))]
  (println "calc")
  (println (time (integrate-mem f 100 1)))
  (println (time (integrate-mem f 200 1)))
  (println (time (integrate-mem f 300 1)))
  (println (time (integrate-mem f 400 1)))
  (println (time (integrate-mem f 500 1)))
  (println "read from mem")
  (println (time (integrate-mem f 9 1))) ; mem ok
  (println (integrate-mem f 9 1))
)

(time (integrate-mem #(* % %) 200 1)) ; stack fault, memoize fault
(time (integrate-mem #(* % %)   9 1)) ; not memed, тк функция каждый раз новая для интерпритатора

