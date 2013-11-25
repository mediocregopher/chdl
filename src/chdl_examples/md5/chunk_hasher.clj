(ns chdl-examples.md5.chunk-hasher
  "the main loop for the md5 function"
  (require [chdl.gamma.core :as core :refer :all]
           [chdl.beta.math :refer :all]
           [chdl.gamma.function :as function]
           [chdl.gamma.types :as t :refer :all]
           [chdl.gamma.chip :as chip :refer :all]
           [chdl.beta.control :as control]
           [chdl.alpha.proto :as proto]))

(defchip chunk-hasher-chip
         :ports [ message  (t/in-sig (t/slog-vec 512))
                  md5-hash (t/in-sig (t/slog-vec 128))])
