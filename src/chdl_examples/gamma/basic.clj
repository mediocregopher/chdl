(ns chdl-examples.gamma.basic
  (:require [chdl.alpha.literal :as lit]
            [chdl.beta.comp :as comp]
            [chdl.beta.math :as math]
            [chdl.gamma.chip :as chip]
            [chdl.gamma.processes :as proc]
            [chdl.gamma.render :as render]))

(def a-of comp/array-of)
(def a-index comp/array-index)
(def downto comp/downto)
(def n lit/num10)
(def assign! comp/assign-signal!)

(def full-adder
  (chip/chip :fullAdder
    :in [[:a :bit] [:b :bit] [:cin :bit]]
    :out [[:s :bit] [:cout :bit]]
    :internal [(comp/signal :axorb :bit)]
    :body [
      (assign! :axorb (math/xor :a :b))
      (assign! :s     (math/xor :axorb :cin))
      (assign! :cout  (math/or (math/and :axorb :cin) (math/and :a :b)))
    ]))

(def width 8)

(defn arr-w [w] (a-of :BIT (downto (n (dec w)) (n 0))))
(def  arr   (arr-w width))
(def  arr++ (arr-w (inc width)))

(def wide-adder
  (chip/chip :main
    :in [[:a arr] [:b arr]]
    :out [[:s arr] [:cout :BIT]]
    :internal [(comp/signal :tmp arr++)]
    :body (flatten [
      (assign! (a-index :tmp 0) (lit/bit 0))
      (assign! :cout (a-index :tmp width))
      (map
        #(chip/chip-inst :fullAdder
          [:a    (a-index :a   %)]
          [:b    (a-index :b   %)]
          [:cin  (a-index :tmp %)]
          [:s    (a-index :s   %)]
          [:cout (a-index :tmp (inc %))])
        (range width))
    ])))

(defn -main [& args]
  (render/vhdl-file "src/chdl_examples/gamma/basic.vhd" full-adder wide-adder))

(comment
  (-main)
)
