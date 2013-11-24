(ns chdl.gamma.types
  "Expand existing literals with types. Also have a special type inferencing beta.comp/sigcon"
  (:require [chdl.alpha.literal :as lit]
            [chdl.beta.math :as m]
            [chdl.beta.comp :as c]
            [chdl.beta.process :as proc]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]
            [chdl.gamma.protocols :as gamma-proto]
            [chdl.gamma.symbols :as symbols]))

(defrecord sym-construct 
  [type f]
  gamma-proto/typed-construct
  (type-info [this] (:type this)))
  
(defrecord sym-construct-value 
  [type f construct]
  gamma-proto/typed-construct
  proto/alpha-item
  (type-info [this] (:type this))
  (to-str [this] (proto/to-str (:value this))))


(defn decorate-type [type-keyword f]
  (fn
    ([]
     (map->sym-construct 
       {:type type-keyword
        :f f}))
    ([& args]
     (map->sym-construct-value
       {:type type-keyword
        :value (apply f args)
        :f f}))))

(def bit (decorate-type :BIT lit/bit))
(def character (decorate-type :CHARACTER lit/character))
(def bool (decorate-type :BOOLEAN lit/bool))
(def bit-vec (decorate-type :BIT_VECTOR lit/bit-vec))
(def integer (decorate-type :INTEGER lit/num10))
(def string (decorate-type :STRING lit/string))

(defn sigcon
  "same as beta.comp/sigcon, but infers type!"
  [sigcon typed-lit]
  (let [var-name (str (gensym (str (name sigcon) "-" (name (:type typed-lit)))))
        construct (if (:value typed-lit)
                (c/sigcon sigcon var-name (:type typed-lit) (:value typed-lit))
                (c/sigcon sigcon var-name (:type typed-lit)))]
    (symbols/map->chdl-symbol (assoc typed-lit :name var-name :construct construct))))


(def variable (partial sigcon :VARIABLE))
(def constant (partial sigcon :CONSTANT))
(def signal (partial sigcon :SIGNAL))


(comment 
  ;; when we define a variable we get a chdl-symbol
  (def some-var
    (variable (bit 0)))

  ;; We can examine the chdl-symbol name with sym-name
  (gamma-proto/sym-name some-var)

  ;;Examine the type with type-info
  (gamma-proto/type-info some-var)

  ;;Examine the chdl construct with to-str
  (proto/to-str (gamma-proto/construct some-var))
  
  ;; Now, whenever we use this chdl-symbol, the to-str function will return the name of the symbol
  (proto/to-str (variable (bit)))

  (proto/to-str (c/sigcon :VARIABLE "foo" :BIT nil))


  )
