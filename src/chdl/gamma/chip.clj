(ns chdl.gamma.chip
  (:require [clojure.walk :refer [walk]]
            [chdl.beta.comp :as comp]
            [chdl.beta.design :as design]
            [chdl.beta.math :as math]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]
            [chdl.gamma.protocols :as gproto]
            [chdl.gamma.core :as core]
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

(defn >!
  "Pretty way of assigning signal. Really just a wrapper over
  chdl.beta.comp/assign-signal!"
  [dst src]
  (comp/assign-signal! dst src))

(defrecord chip-rec
  [cname port-map construct]
  proto/alpha-item
  (to-str [this] (proto/to-str (:construct this))))

( defmacro chip
  "An instantiator for the definition of a new chip entity. This entity has
  input/output/inout signals, possible internal signals, ability to have
  multiple other chips embedded inside of it, processes, etc...

  Example:
  (chip wat
    :ports    [a   (types/in-sig (types/bit))
               b   (types/in-sig (types/bit 0))
               ret (types/out-sig (types/bit))]
    :internal [tmp (types/signal (types/bit 0))]

    (>! tmp (math/xor a b))
    (>! ret (math/not tmp)))"
  [cname & args]
  (let [m                 (chip-def->map args)
        port-bindings     (m :ports)
        internal-bindings (m :internal)
        bindings          (vec (concat port-bindings internal-bindings))
        port-syms         (vec (take-nth 2 port-bindings))
        internal-syms     (vec (take-nth 2 internal-bindings))
        body              (m :body)]
    `(let ~bindings
      (->chip-rec
        ~cname
        ~(reduce #(assoc %1 `(quote ~(first %2)) (first %2)) {}
          (partition 2 port-bindings))
        (expr/concated
          (apply design/entity '~cname (map gproto/construct ~port-syms))
          (design/architecture '~cname :ARCH
            (map gproto/construct ~internal-syms)
            (flatten ~body)))))))

(defmacro defchip
  [cname & args]
  `(def ~cname (chip ~cname ~@args)))

(defmacro chip-inst
  "Used to instantiate a chip entity inside of another chip. You give it the
  name of the chip entity and any port pairs that need to be hooked up"
  [cname ports]
  (let [port-map (:port-map (eval cname))
        port-pairs (map #(let [[dst src] %] [(port-map dst) src])
          (partition 2 ports))]
    `(design/component (name (gensym "CHIP")) ~(name cname) :ARCH ~@port-pairs)))

(comment

  (defchip wat
    :ports [a (types/in-sig (types/bit))
            b (types/in-sig (types/bit 0))
            ret (types/out-sig (types/bit))]

    :internal [tmp  (types/signal (types/bit 0))
               tmp2 (types/signal (types/slog-vec 8 "101010"))]

    (>! tmp (math/vxor a b))
    (>! (core/vec-nth tmp2 0 4) (types/bit-vec 4 "1111"))
    (>! (core/vec-nth tmp2 4 8) (core/vec-nth tmp2 0 4))
    (>! ret (math/vnot tmp)))

  (println (proto/to-str wat))

  (println (proto/to-str
    (chip wut
      :ports [c (types/in-sig (types/bit))
              d (types/in-sig (types/bit))]

      (chip-inst wat [a (core/vec-nth c 0 4) b d]))
  ))
)
