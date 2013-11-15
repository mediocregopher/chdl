(ns chdl.gamma.chip
  (:require [clojure.walk :refer [walk]]
            [chdl.beta.comp :as comp]
            [chdl.beta.design :as design]
            [chdl.beta.math :as math]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]
            [chdl.gamma.types :as types]))

(defn- make-port
  [in out inout]
  (apply comp/port
    (concat
      (map #(vector (first %) :in    (second %)) in)
      (map #(vector (first %) :out   (second %)) out)
      (map #(vector (first %) :inout (second %)) inout))))

(comment

  (defn- chip-def->map
    [body]
    (reduce #(assoc %1 (last (first %2)) (second %2)) {}
      (partition 2 (partition-by keyword? body))))

  (defn- make-port
    [in out inout]
    (apply comp/port
      (concat
        (map #(vector (:name %) :IN    (:type %)) in)
        (map #(vector (:name %) :OUT   (:type %)) out)
        (map #(vector (:name %) :INOUT (:type %)) inout))))

  (defn- make-internal [internal]
    (map #(comp/signal (:name %) (:type %)) internal))

  (defn- symbolize
    "Given an arbitrary sequence, with possibly embedded sequences, traverses
    the whole thing and finds any symbols in it that aren't resolvable and
    quotes them. This is presumably used inside of a defmacro"
    [tree]
    (walk #(cond (and (symbol? %) (nil? (resolve %))) `(quote ~%)
                 (seq? %) (symbolize %)
                 :else %) identity tree))

  (defmacro chip [cname & args]
    (let [m        (chip-def->map args)
          in       `(list ~@(symbolize (m :in [])))
          out      `(list ~@(symbolize (m :out [])))
          inout    `(list ~@(symbolize (m :inout [])))
          internal `(list ~@(symbolize (m :internal [])))
          body     `(list ~@(m :body []))]
      `(expr/concated
        (design/entity '~cname (make-port ~in ~out ~inout))
        (design/architecture :ARCH '~cname
          (make-internal ~internal)
          (flatten ~body)))))

  (println (proto/to-str
  (chip wat
    :in (types/bit a) (types/bit b)
    :out (types/bool ret)
    :inout (types/bit-vec somebus)
    :internal (types/string tmp))))
)

(defn chip
  "An instantiator for the definition of a new chip entity. This entity has
  input/output/inout signals, possible internal signals, ability to have
  multiple other chips embedded inside of it, processes, etc...

  It takes alternating keywords and then sequences to fill those
  keywords. :in describes all the input signals, :out and :inout are similar.
  :internal describes signals and types that are internal to this chip. Finally
  :body describes the actual logic of the chip"
  [cname & b]
  (let [m (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 b))
        in (m :in [])
        out (m :out [])
        inout (m :inout [])
        internal (m :internal [])
        body (m :body [])]
    (expr/concated
      (design/entity cname (make-port in out inout))
      (design/architecture :ARCH cname internal body))))

(defn chip-inst
  "Used to instantiate a chip entity inside of another chip. You give it the
  name of the chip entity and any port pairs that need to be hooked up"
  [cname & ports]
  (apply design/component (name (gensym)) cname :ARCH ports))

(comment

  (proto/to-str
    (make-port
      [[:aIn :bit] [:bIn :bit]]
      [[:aOut :bit] [:bOut :b]]
      [[:bus1 :bit]]))

  (println (proto/to-str
    (chip :testchip
      :in  [[:in1 :bit] [:in2 :bit] [:in3 :bit]]
      :out [[:out1 :bit]]
      :internal [(comp/signal :tmpSign :bit)]
      :body [
        (chip-inst :other-chip [:otherSign :tmpSign])
        (comp/assign-signal! :tmpSign (math/xor :in1 :in2))
        (comp/assign-signal! :out1 (math/xor :in3 :out1))])
      ))

)
