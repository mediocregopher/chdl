(ns chdl.gamma.symbols
  "Define symbols for chdl. CHDL symbols can represent a variable, signal, or constant. Contains Type information"
  (require [chdl.gamma.protocols :as gamma-proto]
           [chdl.alpha.proto :as proto]
           [chdl.alpha.literal :as lit]))

(defrecord chdl-symbol
  [name type f value]
  gamma-proto/typed-construct
  gamma-proto/symbol-value
  proto/alpha-item
  (type-info [this] (:type this))
  (sym-name  [this] (:name this))
  (construct [this] (:construct this))
  (to-str    [this] (:name this)))

