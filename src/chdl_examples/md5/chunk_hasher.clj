(ns chdl-examples.md5.chunk-hasher
  "the main loop for the md5 function"
  (require [chdl.gamma.core :as core :refer :all]
           [chdl.beta.math :refer :all]
           [chdl.gamma.function :as function]
           [chdl.gamma.types :as t :refer :all]
           [chdl.gamma.chip :as chip :refer :all]
           [chdl.beta.control :as control]
           [chdl.alpha.proto :as proto]
           [chdl.gamma.protocols :as gproto]))


(defchip chunk-hasher-chip
  :ports [message  (t/in-sig (t/slog-vec 512))
          md5-hash (t/out-sig (t/slog-vec 128))]
  :internal [M (mapv (fn [_] (t/signal (t/std-uint 32))) (range 16))]

 (map-indexed
   #(>! %2 (slog-vec->std-uint
     (vec-nth message (* 32 %1) (* 32 (inc %1))))) M))

(comment
  (println (gproto/construct chunk-hasher-chip))
)
