(ns chdl.gamma.core
  "Useful functions"
  (require 
    [chdl.alpha.literal :as lit]
    [chdl.alpha.expr :as expr]
    [chdl.alpha.proto :as proto]
    [chdl.beta.math :as math]
    [chdl.beta.comp :as comp]
    [chdl.gamma.types :as t]
    [chdl.gamma.function :as cfn]))

(defn raw-bit-value? [value chdl-symbol]
  {:pre [(= :BIT (:type chdl-symbol))]}
    (math/v= chdl-symbol (lit/bit value)))

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

(def int-range
  (t/decorate-type
    :INTEGER
    (fn
      ([count]
       (comp/to 0 count))
      ([start end]
       (if (< end start)
         (comp/downto start end)
         (comp/to start end))))))

(comment

  (require '[chdl.alpha.proto :as proto])

  (proto/to-str
    (:value (int-range 3)))

  (proto/to-str (int-range 3))
  (:value (int-range 3))

  (def k (int-range 3))
  (type k)
  (type (:value k))
  (proto/to-str k)
  (gamma-proto/to-str (int-range 3))
  (int-range)
  (proto/to-str
    (int-range 3))

  (def k (t/signal (t/bit 1)))
  (proto/to-str k)
  (lit/raw "'event")

  (proto/to-str (event? k))
  (proto/to-str (high? k))
  (proto/to-str (low? k))
  (= :BIT (:type k))


  )
