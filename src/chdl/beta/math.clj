(ns chdl.beta.math
  (:require [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]))

(defn- math-op
  "Given an operator and two operands, represents the syntax '(a op b)', which
  will be used for general math operations"
  [op a b]
  (expr/parend
    (expr/space-sepd (lit/auto-raw a) (lit/raw op) (lit/auto-raw b))))

(defn- math-single-op
  "Like math-op, but only for a single operand in the syntax '(op a')"
  [op a]
  (expr/parend
    (expr/space-sepd (lit/raw op) (lit/auto-raw a))))

(defn- math-distribute
  "Given an operator and at least one operand, represents the syntax
  '(((a op b) op c) op d ...)' etc... for however many operands are given"
  [op a & args]
  (reduce (partial math-op op) (lit/auto-raw a) args))

(def vexp (partial math-distribute :**))
(def vabs (partial math-single-op :abs))
(def vnot (partial math-single-op :not))

(def v* (partial math-distribute :*))
(def vdiv (partial math-distribute "/"))
(def vmod (partial math-distribute :mod))
(def vrem (partial math-distribute :rem))

(def v+ (partial math-distribute :+))
(def v- (partial math-distribute :-))

(def vsll (partial math-distribute :sll))
(def vsrl (partial math-distribute :srl))
(def vsla (partial math-distribute :sla))
(def vsra (partial math-distribute :sra))
(def vrol (partial math-distribute :rol))
(def vror (partial math-distribute :ror))

(def v= (partial math-op :=))
(def v!= (partial math-op :!=))
(def v< (partial math-op <))
(def v<= (partial math-op <=))
(def v> (partial math-op >))
(def v>= (partial math-op >=))

(def vand (partial math-distribute :and))
(def vor (partial math-distribute :or))
(def vnand (partial math-distribute :nand))
(def vnor (partial math-distribute :nor))
(def vxor (partial math-distribute :xor))
(def vxnor (partial math-distribute :xnor))

(def vinc (partial math-distribute :+ (lit/num10 1)))
(def vdec #(math-distribute :- % (lit/num10 1)))

(comment
  (proto/to-str
    (math-distribute :op :a (lit/num2 "010") :c :d :e))
  (proto/to-str (abs (lit/num10 50)))
  (proto/to-str (abs (lit/num10 50)))

  (proto/to-str (inc (lit/num10 50)))
  (proto/to-str (dec (lit/num10 50)))
  (proto/to-str (math-distribute :+ (lit/num10 1) (lit/num10 100)))

  (proto/to-str (exp (lit/num10 50) (lit/num2 "1001") (lit/num8 "7404")))
  (proto/to-str (= (lit/num10 10) (lit/num2 "1010")))

)
