(ns labs.tests
    (:use labs.core)
    (:require [clojure.test :as test]))

(test/deftest tests
    (test/testing
        (test/is                
            (=    
                (get-result
                    (calculate
                        (f-or
                            (constant false)
                            (variable :a)
                        )
                        (variable :a) (constant false)
                    )
                )
                false
            )            
        )

        (test/is
            (=
                (dnf01 dnf00tst)
                (f-not (f-or (f-or (f-not (variable :x)) (variable :y)) (f-not (f-or (f-not (variable :y)) (variable :z)))))
            )
        )

        (test/is
            (=
                (dnf0203 dnf01tst)
                (f-and (f-and (variable :x) (f-not (variable :y))) (f-or (f-not (variable :y)) (variable :z)))
            )
        )
        
        
    )
)

(test/run-tests 'labs.tests)
