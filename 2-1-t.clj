(ns labs.tests
    (:use labs.core)
    (:require [clojure.test :as test]))

(test/deftest tests
    (test/testing
        (test/is
            (= (integrate #(* % %) 9 1) 489/2)
        )

        (test/is
            (= (integrate #(* % %) 2 1) 3N)
        )

        (test/is
            (= (integrate #(* % %) 3 1/2) 73/8)
        )
    )
)

(test/run-tests 'labs.tests)
