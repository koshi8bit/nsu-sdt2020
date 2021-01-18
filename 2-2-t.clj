(ns labs.tests
    (:use labs.core)
    (:require [clojure.test :as test]))

(test/deftest tests
    (test/testing
        (test/is
            (let [integrator (get-integrator (fn [x] (* x x)) 1/100)]
                (= (integrator 3) 180001/20000)
            )
        )
        (test/is
            (let [integrator (get-integrator (fn [x] (* x x)) 1/100)]
                (= (integrator 2) 26667/10000)
            )
        )
        (test/is
            (let [integrator (get-integrator (fn [x] (* x x)) 1/100)]
                (= (integrator 5/2) 41667/8000)
            )
        )
        (test/is
            (let [integrator (get-integrator (fn [x] (* x x)) 1)]
                (= (integrator 5) 85/2)
            )
        )
        (test/is
            (let [integrator (get-integrator (fn [x] (* x x)) 1)]
                (= (integrator 21/10) 6841/2000)
            )
        )
    )
)

(test/run-tests 'labs.tests)
