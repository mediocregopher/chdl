(ns chdl.gamma.chip
  (:require [chdl.beta.comp :as comp]
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

(defn- chip-def->map
  [body]
  (reduce #(assoc %1 (last (first %2)) (second %2)) {}
    (partition 2 (partition-by keyword? body))))

(defn sigdef-quote
  "Takes in a list of type declarations from gamma.type (bit, character,
  etc...), and quotes each one's first argument. This is useful because it lets
  us pass in symbols into the type functions, so later (in make-port) they get
  used as signal declarations"
  [defs]
  `(list
    ~@(map #(cons (first %)
            (cons `'~(second %) ;ermagerd magic!
            (drop 2 %))) defs)))

(comment

  (defn- make-port
    [in out inout]
    (apply comp/port
      (concat
        (map #(vector (:name %) :in    (:type %)) in)
        (map #(vector (:name %) :out   (:type %)) out)
        (map #(vector (:name %) :inout (:type %)) inout))))

  (defmacro chip [cname & args]
    (let [m        (chip-def->map args)
          in       (sigdef-quote (m :in []))
          out      (sigdef-quote (m :out []))
          inout    (sigdef-quote (m :inout []))
          internal (m :internal [])
          body (m :body [])]
      `(expr/concated
        (design/entity '~cname (make-port ~in ~out ~inout))
        (design/architecture :ARCH '~cname ~internal ~body))))

  (println (proto/to-str
  (chip wat
    :in (types/bit a) (types/bit b)
    :out (types/bool ret))))
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
