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

(defn f-inv [expr]
    (cons ::not expr)
)

(defn f-inv? [expr]
    (= (first expr) ::not)
)

(defn f-inv-calc [expr var value]
    (let [res (calculate (rest expr) var value)]
        (if (constant? res)
            (constant (not (constant-value res)))
            (f-inv res)
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

(def calculate_rules
    (list
        [(fn [expr _ _] (constant? expr)) (fn [expr _ _] expr)]
        [(fn [expr v _] (same-variables? expr v)) (fn [_ _ value] value)]
        [(fn [expr _ _] (variable? expr)) (fn [expr _ _] expr)]
        [(fn [expr _ _] (f-inv? expr)) f-inv-calc]
        [(fn [expr _ _] (f-or? expr)) f-or-calc]
        [(fn [expr _ _] (f-and? expr)) f-and-calc]
        [(fn [expr _ _] (f-impl? expr)) f-impl-calc]
        [(fn [expr _ _] (f-xor? expr)) f-xor-calc]
        ))

(defn calculate [expr var value]
    ;;(println expr var value)
    ((some
         (fn [rule]
             (if ((first rule) expr var value)
                 (second rule)
                 false))
         calculate_rules
    )
    expr var value)
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
        (f-inv (f-inv (variable :b)))
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
