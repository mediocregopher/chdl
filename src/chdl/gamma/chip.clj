(ns chdl.gamma.chip
  (:require [chdl.beta.comp :as comp]
            [chdl.beta.design :as design]
            [chdl.beta.math :as math]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]))

(defn- make-port
  [in out inout]
  (apply comp/port
    (concat
      (map #(vector (first %) :in    (second %)) in)
      (map #(vector (first %) :out   (second %)) out)
      (map #(vector (first %) :inout (second %)) inout))))

(defn chip
  [cname & b]
  (let [m (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 b))
        in (m :in [])
        out (m :out [])
        inout (m :inout [])
        internal (m :internal [])
        body (m :body [])]
    (expr/concated
      (design/entity cname (make-port in out inout))
      (design/architecture :ARCH cname internal body))))

(comment

  (proto/to-str
    (make-port
      [[:aIn :bit] [:bIn :bit]]
      [[:aOut :bit] [:bOut :b]]
      [[:bus1 :bit]]))

  (println (proto/to-str
    (chip :testchip
      :in  [[:in1 :bit] [:in2 :bit] [:in3 :bit]]
      :out [[:out1 :bit]]
      :internal [(comp/signal :tmpSign :bit)]
      :body [
        (comp/assign-signal! :tmpSign (math/xor :in1 :in2))
        (comp/assign-signal! :out1 (math/xor :in3 :out1))])
      ))

)
