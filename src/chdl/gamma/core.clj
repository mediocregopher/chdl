(ns chdl.gamma.core
  "Useful functions"
  (require 
    [chdl.alpha.literal :as lit]
    [chdl.alpha.expr :as expr]
    [chdl.beta.math :as math]
    [chdl.beta.comp :as comp]
    [chdl.gamma.types :as t]))

(defn raw-bit-value? [value chdl-symbol]
  {:pre [(= :BIT (:type chdl-symbol))]}
    (math/= chdl-symbol (lit/bit value)))

(def high?
  (t/decorate-type
    :BIT
    (partial raw-bit-value? 1)))

(def low?
  (t/decorate-type
    :BIT
    (partial raw-bit-value? 0)))

(def event? 
  (t/decorate-type
    :BIT
    (fn [chdl-symbol]
      (expr/parend
        (expr/concated chdl-symbol (lit/raw "'event"))))))

(defn vec-nth
  ([var-name i] (comp/paren-call var-name (lit/raw i)))
  ([var-name start end]
    (comp/paren-call var-name (comp/downto (dec end) start))))


(comment 

  (require '[chdl.alpha.proto :as proto])
  (def k (t/signal (t/bit 1)))
  (proto/to-str k)
  (lit/raw "'event")

  (proto/to-str (event? k))
  (proto/to-str (high? k))
  (proto/to-str (low? k))
  (= :BIT (:type k))


  )
