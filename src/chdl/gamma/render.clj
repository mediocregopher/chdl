(ns chdl.gamma.render
  (:require [chdl.alpha.proto :as proto]
            [chdl.alpha.expr :as expr]))

(defn to-vhdl
  "Renders a set of chdl items"
  [& stuff]
  (proto/to-str (apply expr/concated stuff)))

(defn vhdl-file
  "Given a filename and a set of chdl items, renders and writes them to the
  file"
  [fname & stuff]
  (spit fname (apply to-vhdl stuff)))
