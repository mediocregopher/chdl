(ns chdl.gamma.chip
  (:require [clojure.walk :refer [walk]]
            [chdl.beta.comp :as comp]
            [chdl.beta.design :as design]
            [chdl.beta.math :as math]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]
            [chdl.gamma.types :as types]))

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
               (sequential? %) (symbolize %)
               :else %) identity tree))

(defn <!
  "Pretty way of assigning signal. Really just a wrapper over
  chdl.beta.comp/assign-signal!"
  [dst src]
  (comp/assign-signal! dst src))


(defmacro chip
  "An instantiator for the definition of a new chip entity. This entity has
  input/output/inout signals, possible internal signals, ability to have
  multiple other chips embedded inside of it, processes, etc...

  Example:
  (chip wat
    :in (types/bit a) (types/bit b)
    :out (types/bool ret)
    :inout (types/bit-vec somebus)
    :internal (types/string tmp)
    :body
      (<! a ret)
      (chip-inst blah [a b] [c d]))"
  [cname & args]
  (let [m        (chip-def->map args)
        in       `(list ~@(symbolize (m :in [])))
        out      `(list ~@(symbolize (m :out [])))
        inout    `(list ~@(symbolize (m :inout [])))
        internal `(list ~@(symbolize (m :internal [])))
        body     `(list ~@(symbolize (m :body [])))]
    `(expr/concated
      (design/entity '~cname (make-port ~in ~out ~inout))
      (design/architecture :ARCH '~cname
        (make-internal ~internal)
        (flatten ~body)))))

(defn chip-inst
  "Used to instantiate a chip entity inside of another chip. You give it the
  name of the chip entity and any port pairs that need to be hooked up"
  [cname & ports]
  (apply design/component (name (gensym)) cname :ARCH ports))

(comment

  (println (proto/to-str
  (chip wat
    :in (types/bit a) (types/bit b)
    :out (types/bool ret)
    :inout (types/bit-vec somebus)
    :internal (types/string tmp)
    :body
      (<! a ret)
      (chip-inst blah [a b] [c d]))))

)
