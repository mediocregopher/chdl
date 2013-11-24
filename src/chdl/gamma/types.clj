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
(def integer (decorate-type :INTEGER lit/num10))
(def string (decorate-type :STRING lit/string))

(defn bit-vec [size & default]
  (let [typ (c/paren-call :BIT_VECTOR (c/downto (dec size) 0))
        f lit/bit-vec]
    (if (empty? default)
      (map->sym-construct {:type typ :f f})
      (map->sym-construct-value {:type typ :f f :value (apply f default)}))))

(defn vec-nth
  ([var-name i] (c/paren-call var-name (lit/raw i)))
  ([var-name start end] (c/paren-call var-name (c/downto (dec end) start))))

(defn- gen-name [& n]
  (str (gensym
    (apply str (interpose "-" (map name n))))))

(defn sigcon
  "same as beta.comp/sigcon, but infers type!"
  [sigcon typed-lit]
  (let [var-name (gen-name sigcon)
        construct (if (:value typed-lit)
                (c/sigcon sigcon var-name (:type typed-lit) (:value typed-lit))
                (c/sigcon sigcon var-name (:type typed-lit)))]
    (symbols/map->chdl-symbol (assoc typed-lit :name var-name :construct construct))))


(def variable (partial sigcon :VARIABLE))
(def constant (partial sigcon :CONSTANT))
(def signal (partial sigcon :SIGNAL))

(defn dir-sig
  "same as sigcon, but used for port statements which need a direction"
  [dir typed-lit]
  (let [var-name (gen-name :SIGNAL dir)
        construct (apply c/port dir var-name (:type typed-lit)
                    (if (:value typed-lit) [(:value typed-lit)] []))]
    (symbols/map->chdl-symbol
      (assoc typed-lit :name var-name :construct construct))))

(def in-sig (partial dir-sig :IN))
(def out-sig (partial dir-sig :OUT))
(def inout-sig (partial dir-sig :INOUT))

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
