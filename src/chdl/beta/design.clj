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

(comment
  (println (proto/to-str
    (entity :wat (comp/port
      [:inSig :in :BIT]
      [:outSig :out :BIT]))))

  (println (proto/to-str
    (architecture :wut :wat
      [ (comp/signal :tmpSig1 :BIT (lit/bit 0))
        (comp/signal :tmpSig2 :INTEGER) ]
      [ (comp/assign-signal! :tmpSig1 (math/not (lit/raw :inSig)))
        (comp/assign-signal! :outSig (lit/raw :tmpSig1)) ])))
)
