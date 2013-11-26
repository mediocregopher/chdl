(ns chdl.gamma.num-conv
  "Functions for converting numbers to/from std_logic_vector in vhdl"
  (:require
    [chdl.alpha.literal :as lit]
    [chdl.beta.comp :as comp]))

(defn- unsigned [i] (comp/paren-call "UNSIGNED" i))
(defn- signed [i] (comp/paren-call "SIGNED" i))
(defn- to-integer [i] (comp/paren-call "TO_INTEGER" i))
(defn- to-unsigned [i size] (comp/paren-call "TO_UNSIGNED" i size))
(defn- to-signed [i size] (comp/paren-call "TO_SIGNED" i size))

(defn lit-uint->std-uint [n size] (to-unsigned n size))
(defn lit-int->std-int [n size] (to-signed n size))
(defn std-uint->lit-uint [n] (to-integer n))
(defn std-int->lit-int [n] (to-integer n))

(defn slog-vec->std-uint [v] (unsigned v))
(defn slog-vec->std-int [v] (signed v))
(defn std-uint->slog-vec [v] (lit/slog-vec v))
(defn std-int->slog-vec [v] (lit/slog-vec v))

(defn lit-uint->slog-vec [n size]
  (std-uint->slog-vec (lit-uint->std-uint n size)))
(defn lit-int->slog-vec [n size]
  (std-int->slog-vec (lit-int->std-int n size)))
(defn slog-vec->lit-uint [v]
  (std-uint->lit-uint (slog-vec->std-uint v)))
(defn slog-vec->lit-int [v]
  (std-int->lit-int (slog-vec->std-int v)))
