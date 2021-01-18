(ns labs.tests
    (:use labs.core)
    (:require [clojure.test :as test]))

(test/deftest tests
    (test/testing
        (test/is
            (let [f-pred (fn [coll] (>= (heavy-count coll) 5))
                  coll (list (range 2) (range 5) (range 7) (range 10))]
                (=
                    (filter-my 4 f-pred coll)
                    '((0 1 2 3 4) (0 1 2 3 4 5 6) (0 1 2 3 4 5 6 7 8 9))
                )
            )
        )
    )
)

(test/run-tests 'labs.tests)
