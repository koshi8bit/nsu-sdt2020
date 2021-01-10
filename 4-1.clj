(ns labs.core
    (:gen-class))

;; BASIC

(defn constant [value]
    {:pre [(boolean? value)]}
    (list ::const value)
)

(defn constant? [expr]
    (= (first expr) ::const)
)

(defn constant-value [expr]
    {:pre [(constant? expr)]}
    (second expr)
)

(defn variable? [expr]
    (= (first expr) ::var)
)


(defn variable [name]
    {:pre [(keyword? name)]}
    (list ::var name)
)

(defn variable-name [expr]
    {:pre [(variable? expr)]}
    (second expr)
)

(defn same-variables? [var1 var2]
    ;{:pre [(and (variable? var1) (variable? var2))]}
    (and (variable? var1) (variable? var2) (= (variable-name var1) (variable-name var2)))
)

(declare calculate)

;; AND

(defn f-and [expr & rest]
    (cons ::and (cons expr rest))
)

(defn f-and? [expr]
    (= (first expr) ::and)
)

(defn f-and-calc [expr var value]
    (let [res (map #(calculate % var value) (rest expr))]
        (if (every? #(constant? %) res)
            (constant (every? true? (map constant-value res)))
            (if (some #(and (constant? %) (not (constant-value %))) res)
                (constant false)
                (f-and res)
            )
        )
    )
)

;; OR

(defn f-or [expr & rest]
    (cons ::or (cons expr rest))
)

(defn f-or? [expr]
    (= (first expr) ::or)
)

(defn f-or-calc [expr var value]
    (let [res (map #(calculate % var value) (rest expr))]
        (if (every? #(constant? %) res)
            (constant (boolean (some true? (map constant-value res))))
            (if (some #(and (constant? %) (constant-value %)) res)
                (constant true)
                (f-or res)
            )
        )
    )
)

;; NOT

(defn f-not [expr]
    (cons ::not expr)
)

(defn f-not? [expr]
    (= (first expr) ::not)
)

(defn f-not-calc [expr var value]
    (let [res (calculate (rest expr) var value)]
        (if (constant? res)
            (constant (not (constant-value res)))
            (f-not res)
        )
    )
)

;; ->

(defn f-impl [a b]
    (cons ::impl (list a b))
)

(defn f-impl? [expr]
    (= (first expr) ::impl)
)

(defn f-impl-calc [expr var value]
    (let [res (map #(calculate % var value) (rest expr))]
        (if (every? #(constant? %) res)
            (constant (not (and (constant-value (first res)) (not (constant-value (second res))))))
            (f-impl (first res) (second res))
        )
    )
)

;; XOR
(defn xor [a b]
    (not= a b)
)

(defn f-xor [a b]
    (cons ::xor (list a b))
)

(defn f-xor? [expr]
    (= (first expr) ::xor)
)

(defn f-xor-calc [expr var value]
    (let [res (map #(calculate % var value) (rest expr))]
        (if (every? #(constant? %) res)
            (constant
                (xor
                    (constant-value (first res))
                    (constant-value (second res))
                )
            )
            (f-xor (first res) (second res))
        )
    )
)

;; parser

(def calculate-rules
    (list
        [(fn [expr _ _] (constant? expr)) (fn [expr _ _] expr)]
        [(fn [expr v _] (same-variables? expr v)) (fn [_ _ value] value)]
        [(fn [expr _ _] (variable? expr)) (fn [expr _ _] expr)]
        [(fn [expr _ _] (f-not? expr)) f-not-calc]
        [(fn [expr _ _] (f-or? expr)) f-or-calc]
        [(fn [expr _ _] (f-and? expr)) f-and-calc]
        [(fn [expr _ _] (f-impl? expr)) f-impl-calc]
        [(fn [expr _ _] (f-xor? expr)) f-xor-calc]
        ))

(defn calculate [expr var value]
    ;;(println expr var value)
    (
        (some
         (fn [rule]
             (if ((first rule) expr var value)
                 (second rule)
                 false))
         calculate-rules
        )
        expr var value
    )
)

(defn get-result [expr]
    (if (constant? expr)
        (constant-value expr)
        expr
    )
)

;; exec

(println "1" (get-result (calculate
    (f-xor
        (variable :a)
        (f-not (f-not (variable :b)))
    )
    (variable :a) (constant true)))
)

(println "2" (get-result (calculate
    (f-or
        (constant true)
        (variable :c)
    )
    (variable :a) (constant false)))
)

(println "3" (get-result
        (calculate
            (f-and (variable :a) (variable :b))
            (variable :a) (constant true)
        )
    )
)

;;1 (:labs.core/xor (:labs.core/const true) (:labs.core/not :labs.core/not :labs.core/var :b))
;;2 true
;;3 (:labs.core/and ((:labs.core/const true) (:labs.core/var :b)))

;; DNF

(def dnf00tst
    (f-not (f-or (f-impl (variable :x) (variable :y)) (f-not (f-impl (variable :y) (variable :z)))))
)
(println "dnf00" dnf00tst)

(def dnf99tst
    (f-or (f-and (variable :x) (f-not (variable :y))) (f-and (variable :x) (f-not (variable :y)) (variable :z)))
)
;;(println "dnf99" dnf99tst)
;;(dnf (f-or (variable :x) (f-or (f-not (variable :y)) (variable :y))))

(declare simplification)
(def dnf01-rules
    (list
        [(fn [expr] (constant? expr)) (fn [expr] expr)]
        [(fn [expr] (variable? expr)) (fn [expr] expr)]
        [(fn [expr] (f-not? expr)) (fn [expr] (f-not (simplification (rest expr))))]
        [(fn [expr] (f-or? expr)) (fn [expr] (apply f-or (map #(simplification %) (rest expr))))]
        [(fn [expr] (f-and? expr)) (fn [expr] (apply f-and (map #(simplification %) (rest expr))))]
        [(fn [expr] (f-impl? expr)) (fn [expr] (f-or (f-not (simplification (nth expr 1))) (simplification (nth expr 2))))]
        [(fn [expr] (f-xor? expr)) (fn [expr] (f-or
                                                    (f-and
                                                        (f-not (simplification (nth expr 1)))
                                                        (simplification (nth expr 2))
                                                    )
                                                    (f-and
                                                        (simplification (nth expr 1))
                                                        (f-not (simplification (nth expr 2)))
                                                    )))]
    )
)

(defn dnf01 [expr]
    (
        (some
            (fn [rule]
               (if ((first rule) expr)
                 (second rule)
                 false)
            )
            dnf01-rules
        )
        expr
    )
)


(def dnf01tst
    (->>
        dnf00tst
        dnf01
        get-result
    )
)
(println "dnf01" dnf01tst)

(defn dnf0203 [expr]
    (if (f-not? expr)
        (let [expr2 (rest expr)]
            (cond
                (f-not? expr2) (dnf0203 (rest expr2))
                (f-or? expr2) (apply f-and (map #(dnf0203 (f-not %)) (rest expr2)))
                (f-and? expr2) (apply f-or (map #(dnf0203 (f-not %)) (rest expr2)))
                :else (f-not expr2)
            )
        )
        (if (variable? expr)
            expr
            (cons (first expr) (map #(dnf0203 %) (rest expr)))
        )
    )
)

(def dnf0203tst
    (->>
        dnf01tst
        dnf0203
        get-result
    )
)
(println "dnf0203" dnf0203tst)

(defn conjj [expr1 expr2]
    (if (f-or? expr1)
        (if (f-or? expr2)
            (for [x (rest expr1) y (rest expr2)] (f-and x y))
            (for [x (rest expr1)] (f-and x expr2)))
        (if (f-or? expr2)
            (for [x (rest expr2)] (f-and expr1 x))
            (f-and expr1 expr2)
        )
    )
)

(defn dnf04 [expr]
    (if true
        expr
        (cond
            (f-or? expr) (apply f-or (map dnf04 (rest expr)))
            (f-and? expr) (reduce #(apply f-or (conjj %1 %2)) (map dnf04 (rest expr)))
            :else expr
        )
    )
)

(def dnf04tst
    (->>
        dnf0203tst
        dnf04
        get-result
    )
)
(println "dnf04" dnf04tst)


(defn dnf05 [expr]
    (cond
        (f-or? expr)
            (apply f-or
                (reduce
                    #(if (f-or? %2) (concat %1 (rest %2)) (cons %2 %1))
                    (cons '() (distinct (map dnf05 (rest expr))))
                )
            )
        (f-and? expr)
            (apply f-and
                (reduce
                    #(if (f-and? %2) (concat %1 (rest %2)) (cons %2 %1))
                    (cons '() (distinct (map dnf05 (rest expr))))
                )
            )

        :else expr
    )
)

(def dnf05tst
    (->>
        dnf04tst
        dnf05
        get-result
    )
)
(println "dnf05" dnf05tst)

(defn dnf [expr]
    (->>
        expr      ;; Далее будет реализована инструкция с вики https://ru.wikipedia.org/wiki/%D0%94%D0%B8%D0%B7%D1%8A%D1%8E%D0%BD%D0%BA%D1%82%D0%B8%D0%B2%D0%BD%D0%B0%D1%8F_%D0%BD%D0%BE%D1%80%D0%BC%D0%B0%D0%BB%D1%8C%D0%BD%D0%B0%D1%8F_%D1%84%D0%BE%D1%80%D0%BC%D0%B0
        dnf01     ;; Избавиться от всех логических операций, содержащихся в формуле, заменив их основными: конъюнкцией, дизъюнкцией, отрицанием
        dnf0203   ;; Раскрываем скобки и избавляемся от знаков двойного отрицания
        dnf04     ;; Применить, если нужно, к операциям конъюнкции и дизъюнкции свойства дистрибутивности
        dnf05     ;; Используем идемпотентность конъюкции
    )
)

(println "lol" dnf99tst)
(println "source" dnf00tst)
(println "dnf" (dnf dnf00tst))
(println "dnf calc" (get-result
        (calculate
            (dnf dnf00tst)
            (variable :x) (constant true)
        )
    )
)

;;source (:labs.core/not :labs.core/or (:labs.core/impl (:labs.core/var :x) (:labs.core/var :y)) (:labs.core/not :labs.core/impl (:labs.core/var :y) (:labs.core/var :z)))
;;dnf (:labs.core/or (:labs.core/and (:labs.core/var :x) (:labs.core/not :labs.core/var :y)) (:labs.core/and (:labs.core/var :x) (:labs.core/not :labs.core/var :y) (:labs.core/var :z)))
;;dnf calc (:labs.core/or (:labs.core/and (:labs.core/not :labs.core/var :y)) (:labs.core/and (:labs.core/not :labs.core/var :y) (:labs.core/var :z)))
