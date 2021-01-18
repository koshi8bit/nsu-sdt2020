(ns labs.tests
    (:use labs.core)
    (:require [clojure.test :as test]))

(test/deftest tests
    (test/testing
        (test/is
            (let [f-pred (fn [coll] (>= (heavy-count coll) 3))
                  coll (list (range 1) (range 2) (range 3) (range 4))]
                (=
                    (filter-my 3 2 f-pred coll)
                    '((0 1 2) (0 1 2 3))
                )
            )
        )
    )
)

(test/run-tests 'labs.tests)
