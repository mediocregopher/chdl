(ns chdl.gamma.chip
  (:require [clojure.walk :refer [walk]]
            [chdl.beta.comp :as comp]
            [chdl.beta.design :as design]
            [chdl.beta.math :as math]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]
            [chdl.gamma.types :as types]))

(defn- chip-def->map
  "Partitions a given sequence into 2-tuples, where each key is a keyword and
  each value can be anything. The values are assoc'd to the key in the return
  map. Any values not associated with a keyword get associated with the :body
  keyword in the return map

  Example:
  (chip-def->map [:in [1 2 3] :out [4 5 6] 7 8 9])
    => {:in (1 2 3) :out (4 5 6) :body (7 8 9)}
  "
  [body]
  (first (reduce
    (fn [last-state el]
      (let [[m prev-key] last-state]
        (cond (keyword? el) [m el]
              (nil? prev-key)
                [(assoc m :body (apply vector (concat (m :body []) [el]))) nil]
              :else [(assoc m prev-key el) nil])))
    [{} nil] body)))

(defn- make-port-vecs
  "Given a vector of alternating names and type decorators, and a direction,
  creates the sequence of vectors that should be passed into beta.comp/port"
  [ntds dir]
  (map
    (fn [ntd]
      (let [[n td] ntd]
        (apply vector n dir (:type td) (if (:value td) [(:value td)] []))))
    (partition 2 ntds)))

(defn- make-port
  [in out inout]
  (apply comp/port
    (concat
      (make-port-vecs in :IN)
      (make-port-vecs out :OUT)
      (make-port-vecs inout :INOUT))))

(defn- make-internal [internal]
  (map
    #(apply comp/signal (first %) (:type (second %))
      (if (:value (second %)) [(:value (second %))] '()))
    (partition 2 internal)))

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
    :in       [a (types/bit)
               b (types/bit 0)]
    :out      [ret (types/bit)]
    :internal [tmp (types/bit 0)]

    (<! tmp (math/xor a b))
    (<! ret (math/not tmp)))"
  [cname & args]
  (let [m        (chip-def->map args)
        in       (symbolize (m :in []))
        out      (symbolize (m :out []))
        inout    (symbolize (m :inout []))
        internal (symbolize (m :internal []))
        body     (symbolize (m :body []))]
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
    :in       [a (types/bit)
               b (types/bit 0)]
    :out      [ret (types/bit)]
    :internal [tmp (types/bit 0)]

    (<! tmp (math/xor a b))
    (<! ret (math/not tmp)))))
)
