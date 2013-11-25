(ns chdl.gamma.function
  "Inline functions"
  (:require [chdl.gamma.types :as t]))

(defmacro chdl-fn 
  "will returns a inline function"
  [type args & body]
    `(t/decorate-type
       ~type
       (fn ~args 
         ~@body)))

(defmacro defchdl-fn 
  "will returns a inline function"
  [name type args & body]
  `(def ~name (apply chdl-fn type args body)))
