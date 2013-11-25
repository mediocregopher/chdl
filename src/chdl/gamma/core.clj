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

(defn- unsigned [i] (comp/paren-call "UNSIGNED" i))
(defn- signed [i] (comp/paren-call "SIGNED" i))
(defn- to-integer [i] (comp/paren-call "TO_INTEGER" i))
(defn- to-unsigned [i size] (comp/paren-call "TO_UNSIGNED" i size))
(defn- to-signed [i size] (comp/paren-call "TO_SIGNED" i size))

(defn lit-uint->std-uint [n size] (to-unsigned n size))
(defn lit-int->std-int [n size] (to-signed n size))
(defn std-uint->lit-uint [n] (to-integer n))
(defn std-int->lit-int [n] (to-integer n))

(defn slog-vec->std-uint [v] (unsigned v))
(defn slog-vec->std-int [v] (signed v))
(defn std-uint->slog-vec [v] (lit/slog-vec v))
(defn std-int->slog-vec [v] (lit/slog-vec v))

(defn lit-uint->slog-vec [n size]
  (std-uint->slog-vec (lit-uint->std-uint n size)))
(defn lit-int->slog-vec [n size]
  (std-int->slog-vec (lit-int->std-int n size)))
(defn slog-vec->lit-uint [v]
  (std-uint->lit-uint (slog-vec->std-uint v)))
(defn slog-vec->lit-int [v]
  (std-int->lit-int (slog-vec->std-int v)))


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
