(ns chdl-examples.md5.main-loop
  "the main loop for the md5 function"
  (require [chdl.gamma.core :as core :refer :all]
           [chdl.beta.math :refer :all]
           [chdl.gamma.function :as function]
           [chdl.gamma.types :as t :refer :all]
           [chdl.gamma.chip :as chip :refer :all]
           [chdl.beta.control :as control]
           [chdl.alpha.proto :as proto]))



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

(def Ms (map
  #(signal (slog-vec 32 (vec-nth in-512 (* 32 %) (* 32 (inc %)))))
  (range 0 16)))

(defn pass-maker [pass-number body]
  (let [cname
         (gensym (str "chip-" pass-number "-"))]
    (chip/chip
      cname
      :ports  [    A (t/in-sig (t/std-uint 32))
                     B (t/in-sig (t/std-uint 32))
                     C (t/in-sig (t/std-uint 32))
                     D (t/in-sig (t/std-uint 32))
                     M (t/in-sig (t/slog-vec 32))
                     Ao (t/out-sig (t/std-uint 32))
                     Bo (t/out-sig (t/std-uint 32))
                     Co (t/out-sig (t/std-uint 32))
                     Do (t/out-sig (t/std-uint 32))]
      (eval body))))

(defn leftrotate [A F M i] ;i is a clojure int, not a chdl symbol
  (vrol (v+ A F (K i) M) (s i)))

(defn g-maker [i]
  (cond
    (< 16 i) i
    (< 32 i) (vmod (v+ (v* std-5 (lit-uint->std-uint i 32)) std-1) std-16)
    (< 48 i) (vmod (v+ (v* std-3 (lit-uint->std-uint i 32)) std-5) std-16)
    (< 64 i) (vmod (v* std-7 (lit-uint->std-uint i 32)) std-16)))

(defn F-maker [A B C D i]
  (cond
    (< 16 i) (vor (vand B C) (vand (vnot B) D))
    (< 32 i) (vor (vand D B) (vand (vnot D) C))
    (< 48 i) (vxor B (vxor C D))
    (< 64 i) (vxor C (vor B (vnot D)))))

(defn pass-body [pass-number]
  `[ (>! dTemp D)
     (>! Do C)
     (>! Co Bo)
     (>! Bo (leftrotate A (F-maker A B C D ~pass-number) M ~pass-number))
     (>! dTemp D)])

(comment

  (macroexpand-1 '(pass-maker 1 "1234444") )

  (clojure.pprint/pprint 
    (pass-body 1))

  (proto/to-str (t/slog-vec 4 "1"))
  (:type (t/slog-vec 4 "1"))
  (:type (t/slog-vec 4)) 
  (:type (t/bit))
  (require 'clojure.pprint)
  (clojure.pprint/pprint
  ;(for i (range 63))
  (pass-maker i "1234" `[:foo :bar (~(F i) B C D)]))


  (map #(apply chip/chip) [
                           ['wat
                            :ports [a (t/in-sig (t/bit))
                                      out (t/out-sig (t/bit))]
                            '(lit/raw "asdf")]])
)
