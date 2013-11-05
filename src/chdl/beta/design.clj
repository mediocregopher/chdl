(ns chdl.beta.design
  (:require [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]
            [chdl.beta.comp :as comp]
            [chdl.beta.math :as math]))

(defn- design-unit
  "Represents a general design unit of the form:
  THING name IS stuff BEGIN more stuff END THING name"
  [top-typ bottom-typ pre-beg post-beg]
  (expr/newlined (expr/newlined
    (expr/semicolond
      (expr/concated
        (expr/newlined
          (expr/space-sepd
            top-typ
            (lit/raw :IS)))
        (apply expr/concated
          (map #(expr/tabd (expr/newlined (expr/semicolond %1))) pre-beg))
        (expr/newlined (lit/raw :BEGIN))
        (apply expr/concated
          (map #(expr/tabd (expr/newlined (expr/semicolond %1))) post-beg))
        (expr/space-sepd
          (lit/raw :END)
          bottom-typ))))))

(defn entity
  "Represents an entity with the given ports. Entities are simply an interface
  declaration, their implementation is in the architecture"
  [name ports]
  (let [name-decl (expr/space-sepd (lit/raw :ENTITY) (lit/raw name))]
    (design-unit name-decl name-decl [ports] [])))

(defn architecture
  [of name pre-beg post-beg]
  (design-unit
    (expr/space-sepd
      (lit/raw :ARCHITECTURE)
      (lit/raw name)
      (lit/raw :OF)
      (lit/raw of))
    (expr/space-sepd
      (lit/raw :ARCHITECTURE)
      (lit/raw name))
    pre-beg
    post-beg))


(defn component
  "Given a component's individual name, the name of the actual component, the
  architecture for it, and a vector pairs for port mappings, represents the
  'cname : ENTITY vname(arch) PORT MAP(i0 => o0, i1 => o1)' syntax"
  [vname cname arch & ports]
  (expr/space-sepd
    (lit/raw vname)
    (lit/raw \:)
    (lit/raw :ENTITY)
    (comp/paren-call cname (lit/raw arch))
    (apply comp/paren-call "PORT MAP"
      (map #(expr/space-sepd
              (lit/raw (first %)) (lit/raw :=>) (lit/raw (second %)))
           ports))))

(comment
  (println (proto/to-str
    (entity :wat (comp/port
      [:inSig :in :BIT]
      [:outSig :out :BIT]))))

  (println (proto/to-str
    (architecture :wut :wat
      [ (comp/signal :tmpSig1 :BIT (lit/bit 0))
        (comp/signal :tmpSig2 :INTEGER) ]
      [ (comp/assign-signal! :tmpSig1 (math/not :inSig))
        (comp/assign-signal! :outSig :tmpSig1) ])))


  (println (proto/to-str
    (component :C1 :LargeFlipFlop :ARCH [:clk :clk] [:q :q1] [:d :d1])))
)
