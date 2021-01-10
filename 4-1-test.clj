(ns labs.tests
    (:use labs.core)
    (:require [clojure.test :as test]))

(test/deftest tests
    (test/testing
        (test/is
            (get-result
                (calculate
                    (f-or
                        (constant true)
                        (variable :c)
                    )
                    (variable :a) (constant false)
                )
            )
        )
    )
)

(test/run-tests 'labs.tests)
