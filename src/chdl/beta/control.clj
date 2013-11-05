(ns chdl.beta.control
  "Control structures for vhdl"
  (:require 
    [chdl.gamma.types :as t]
    [chdl.alpha.literal :as lit]
    [chdl.beta.math :as m]
    [chdl.beta.comp :as c]
    [chdl.beta.process :as proc]
    [chdl.alpha.expr :as expr]
    [chdl.alpha.proto :as proto]))

(proto/to-str 
  (expr/space-sepd
    (lit/raw "something")
    (lit/raw "something")))

(defn if-vhdl 
  "If control structure, it can't be called just if, because if is a special form"
  ([predicate then-clauses else-clauses] 
   (expr/concated
     (expr/newlined
       (expr/space-sepd
         (lit/raw "if")
         (expr/parend predicate)
         (lit/raw "then")))
     (expr/tabd (apply c/do-statements then-clauses))
     (expr/newlined (lit/raw "else"))
     (expr/tabd (apply c/do-statements else-clauses))
     (expr/semicolond (lit/raw "end if"))))
  ([predicate then-clauses]
   (expr/concated
     (expr/newlined
       (expr/space-sepd
         (lit/raw "if")
         (expr/parend predicate)
         (lit/raw "then")))
     (expr/tabd (apply c/do-statements then-clauses))
     (expr/semicolond (lit/raw "end if")))))

(comment 
  (println 
  (proto/to-str
    (if-vhdl (lit/raw "foo = bar") [(lit/raw "something")] [(lit/raw "something else")])))
  (println 
  (proto/to-str
    (if-vhdl (lit/raw "foo = bar") [(lit/raw "something")])))
    )
