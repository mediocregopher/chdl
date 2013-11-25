(ns chdl.gamma.process
  (:require
    [chdl.gamma.types :as t]
    [chdl.beta.comp :as c]
    [chdl.beta.process :as proc]
    [chdl.alpha.proto :as proto]
    [chdl.gamma.protocols :as gamma-proto]))

(defmacro let-proc [bindings & body]
  (let [[bindings sensitivity] (split-with (partial not= :watch) bindings)
        sensitivity (vec (rest sensitivity))
        symbols (vec (take-nth 2 bindings))]
    `(proc/process
      (let ~(vec bindings)
         {:sensitivity-list ~sensitivity
          :declarations (map gamma-proto/construct ~symbols)
          :body ~(vec body)}))))
(comment

(def clk (t/signal (t/bit 0)))
(def rst (t/signal (t/bit 0)))


(println
  (proto/to-str
  (let-proc
    [some-variable (t/variable (t/bit 0))
     some-sig (t/signal (t/bit 0))
     :watch clk rst]
    (c/paren-call :write some-variable (c/paren-call :String' (lit/string "Hello world!"))))))
)
