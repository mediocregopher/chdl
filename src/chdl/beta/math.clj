(ns chdl.beta.math
  (:require [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]))

(defn- math-op
  "Given an operator and two operands, represents the syntax '(a op b)', which
  will be used for general math operations"
  [op a b]
  (expr/parend
    (expr/space-sepd a (lit/raw op) b)))

(defn- math-single-op
  "Like math-op, but only for a single operand in the syntax '(op a')"
  [op a]
  (expr/parend
    (expr/space-sepd (lit/raw op) a)))

(defn- math-distribute
  "Given an operator and at least one operand, represents the syntax
  '(((a op b) op c) op d ...)' etc... for however many operands are given"
  [op a & args]
  (reduce (partial math-op op) a args))

(def exp (partial math-distribute :**))
(def abs (partial math-single-op :abs))
(def not (partial math-single-op :not))

(def * (partial math-distribute :*))
(def / (partial math-distribute "/"))
(def mod (partial math-distribute :mod))
(def rem (partial math-distribute :rem))

(def + (partial math-distribute :+))
(def - (partial math-distribute :-))

(def sll (partial math-distribute :sll))
(def srl (partial math-distribute :srl))
(def sla (partial math-distribute :sla))
(def sra (partial math-distribute :sra))
(def rol (partial math-distribute :rol))
(def ror (partial math-distribute :ror))

(def = (partial math-op :=))
(def != (partial math-op :!=))
(def < (partial math-op <))
(def <= (partial math-op <=))
(def > (partial math-op >))
(def >= (partial math-op >=))

(def and (partial math-distribute :and))
(def or (partial math-distribute :or))
(def nand (partial math-distribute :nand))
(def nor (partial math-distribute :nor))
(def xor (partial math-distribute :xor))
(def xnor (partial math-distribute :xnor))

(def inc (partial math-distribute :+ (lit/num10 1)))
(def dec #(math-distribute :- % (lit/num10 1)))

(comment
  (proto/to-str
    (apply math-distribute :op (map lit/raw [:a :b :c :d :e])))
  (proto/to-str (abs (lit/num10 50)))
  (proto/to-str (abs (lit/num10 50)))

  (proto/to-str (inc (lit/num10 50)))
  (proto/to-str (dec (lit/num10 50)))
  (proto/to-str (math-distribute :+ (lit/num10 1) (lit/num10 100)))

  (proto/to-str (exp (lit/num10 50) (lit/num2 "1001") (lit/num8 "7404")))
  (proto/to-str (= (lit/num10 10) (lit/num2 "1010")))

)
