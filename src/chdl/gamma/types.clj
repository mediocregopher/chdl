(ns chdl.gamma.types
  "Expand existing literals with types. Also have a special type inferencing beta.comp/sigcon"
  (:require [chdl.alpha.literal :as lit]
            [chdl.beta.math :as m]
            [chdl.beta.comp :as c]
            [chdl.beta.process :as proc]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]))

(defn decorate-type [type-keyword f]
  (fn
    ([]
     {:type type-keyword})
    ([& args]
      (if (symbol? (first args))
        {:type type-keyword
         :name (first args)}
        {:type type-keyword
         :value (apply f args)}))))

(def bit (decorate-type :BIT lit/bit))
(def character (decorate-type :CHARACTER lit/character))
(def bool (decorate-type :BOOLEAN lit/bool))
(def bit-vec (decorate-type :BIT_VECTOR lit/bit-vec))
(def integer (decorate-type :INTEGER lit/num10))
(def string (decorate-type :STRING lit/string))

;(defmacro sigcon [sigcon value]
;  (let [var-name (str (gensym "variable"))]
;    (if (seq? value)
;      `[(lit/raw ~var-name) (c/sigcon ~sigcon ~var-name (:type (meta ~(first value))) ~value)]
;      `[(lit/raw ~var-name) (c/sigcon ~sigcon ~var-name (:type (meta ~value)))])))

(defmacro ^:dynamic variable [value]
  `(sigcon :VARIABLE ~value))

(defmacro constant [value]
  `(sigcon :CONSTANT ~value))

(defmacro signal [value]
  `(sigcon :SIGNAL ~value))


(defn sigcon
  "same as beta.comp/sigcon, but infers type!"
  [sigcon typed-lit]
  (let [var-name (str (gensym (str (name sigcon) "-" (name (:type typed-lit)))))]
    (if (contains? typed-lit :value)
      [(lit/raw var-name) (c/sigcon sigcon var-name (:type typed-lit) (:value typed-lit))]
      [(lit/raw var-name) (c/sigcon sigcon var-name (:type typed-lit))])))

(def variable (partial sigcon :VARIABLE))
(def constant (partial sigcon :CONSTANT))
(def signal (partial sigcon :SIGNAL))

(map proto/to-str (variable (bit 0)))
