(ns chdl.gamma.protocols
  "Protocols added by the gamma layer")

(defprotocol typed-construct
  (type-info [this] "Returns the type info of the symbol"))

(defprotocol symbol-value
  (sym-name ^String [this] "Returns the name of the symbol being used. Symbols can be variables or signals")
  (construct [this] "Returns the literal that constructs the symbol"))

