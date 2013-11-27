(ns chdl-examples.md5.main-loop
  "the main loop for the md5 function"
  (require [chdl.gamma.core :as core :refer :all]
           [chdl.beta.math :refer :all]
           [chdl.gamma.function :refer :all]
           [chdl.gamma.types :refer :all :as t]
           [chdl.gamma.chip :refer :all]
           [chdl.gamma.num-conv :refer :all]
           [chdl.gamma.protocols :as gproto]
           [chdl.gamma.render :as render]))



(def s [ 7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
         5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
         4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
         6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21 ])

(def K (vec (map #(lit-uint->std-uint % 32)
       [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
         0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
         0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
         0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
         0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
         0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
         0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
         0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
         0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
         0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
         0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
         0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
         0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
         0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
         0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
         0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 ])))

(def a0 (lit-uint->std-uint 0x67452301 32))
(def b0 (lit-uint->std-uint 0xefcdab89 32))
(def c0 (lit-uint->std-uint 0x98badcfe 32))
(def d0 (lit-uint->std-uint 0x10325476 32))

(def std-1 (lit-uint->std-uint 1 32))
(def std-3 (lit-uint->std-uint 3 32))
(def std-5 (lit-uint->std-uint 5 32))
(def std-7 (lit-uint->std-uint 7 32))
(def std-16 (lit-uint->std-uint 16 32))

(defn g-maker [i]
  (cond
    (> 16 i) i
    (> 32 i) (mod (+ (* 5 i) 1) 16)
    (> 48 i) (mod (+ (* 3 i) 5) 16)
    (> 64 i) (mod (* 7 i) 16)))

(defn leftrotate [A F Mg i] ;i is a clojure int, not a chdl symbol
  (vrol (v+ A F (K i) Mg) (s i)))

(defn F-maker [B C D i]
  (cond
    (> 16 i) (vor (vand B C) (vand (vnot B) D))
    (> 32 i) (vor (vand D B) (vand (vnot D) C))
    (> 48 i) (vxor B (vxor C D))
    (> 64 i) (vxor C (vor B (vnot D)))))

(defn pass-maker [pass-number]
    (chip
      :ports  [A (t/in-sig (t/std-uint 32))
               B (t/in-sig (t/std-uint 32))
               C (t/in-sig (t/std-uint 32))
               D (t/in-sig (t/std-uint 32))
               Mg (t/in-sig (t/std-uint 32))
               Ao (t/out-sig (t/std-uint 32))
               Bo (t/out-sig (t/std-uint 32))
               Co (t/out-sig (t/std-uint 32))
               Do (t/out-sig (t/std-uint 32))]
      :internal [ dTemp (t/signal (t/std-uint 32))]
      (>! dTemp D)
      (>! Do C)
      (>! Co B)
      (>! Bo (v+ B (leftrotate A (F-maker B C D pass-number) Mg pass-number)))
      (>! dTemp D)))

(def pass-chips (map pass-maker (range 64)))

(defn pass-input [Sin Ss i]
  (if (zero? i) Sin (Ss i)))

(defn pass-output [Sout Ss i]
  (if (= 63 i) Sout (Ss (inc i))))

(defchip chunk-hasher-chip
      :ports [Ain      (t/in-sig (t/std-uint 32))
              Bin      (t/in-sig (t/std-uint 32))
              Cin      (t/in-sig (t/std-uint 32))
              Din      (t/in-sig (t/std-uint 32))
              message  (t/in-sig (t/slog-vec 512))
              Aout     (t/out-sig (t/std-uint 32))
              Bout     (t/out-sig (t/std-uint 32))
              Cout     (t/out-sig (t/std-uint 32))
              Dout     (t/out-sig (t/std-uint 32))]

      :internal [;breaking up the message
                 M (mapv (fn [_] (t/signal (t/std-uint 32))) (range 16))
                 ;Making the glue
                 As (mapv (fn [_] (t/signal (t/std-uint 32))) (range 64))
                 Bs (mapv (fn [_] (t/signal (t/std-uint 32))) (range 64))
                 Cs (mapv (fn [_] (t/signal (t/std-uint 32))) (range 64))
                 Ds (mapv (fn [_] (t/signal (t/std-uint 32))) (range 64))]

      (map-indexed
        #(>! %2 (slog-vec->std-uint
          (vec-nth message (* 32 %1) (* 32 (inc %1))))) M)

      (map-indexed
        #(chip-inst %2 [A (pass-input Ain As %1)
                        B (pass-input Bin Bs %1)
                        C (pass-input Cin Cs %1)
                        D (pass-input Din Ds %1)
                        Mg (M (g-maker %1))
                        Ao (pass-output Aout As %1)
                        Bo (pass-output Bout Bs %1)
                        Co (pass-output Cout Cs %1)
                        Do (pass-output Dout Ds %1)])
        pass-chips)
)

(comment
  (println (gproto/construct chunk-hasher-chip))
  (println (gproto/construct (first pass-chips)))
  (render/vhdl-file "/tmp/md5.vhdl"  pass-chips chunk-hasher-chip)
)
