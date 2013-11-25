(ns chdl.alpha.literal
  (:require [chdl.alpha.proto :as proto]
            [clojure.string :as s]))

(defn- gen-to-str
  "Takes a value which can be of various types and outputs a string. Does a
  little more work then str does though"
  [el]
  (cond
    (keyword? el) (name el)
    (string? el) el
    (char? el) (str el)
    (number? el) (str el)
    (symbol? el) (name el)
    :else (do (println "Could not convert" el "to string") "WAT")))

(proto/defalpha-item raw
  "Given a string or a keyword, this will represent just that exact value, no
  more no less"
  [el]
    (to-str [_] (gen-to-str el)))

(defn auto-raw
  "Given any clojure value, if it's not already an alpha-item this will wrap it
  in a raw call"
  [el]
  (if (satisfies? proto/alpha-item el) el (raw el)))

(proto/defalpha-item character
  "Given a character, represents that single
  character"
  [ch]
    (to-str [_] (str "'" ch "'")))

(proto/defalpha-item numeric
  "Given a base and a number in that base as a string, represents that number"
  [base n]
    (to-str [_] (str base "#" n "#")))

(defn num2 [n] (numeric 2 n))
(defn num8 [n] (numeric 8 n))
(defn num10 [n] (numeric 10 n))
(defn num16 [n] (numeric 16 n))

(proto/defalpha-item string
  "Given a string, represents just that string with internal \" replaced with
  \"\""
  [s]
    (to-str [_] (str "\"" (s/replace s #"\"" "\"\"") "\"")))

(proto/defalpha-item bit-string
  "Given a base identifier and a string of numbers/characters in that base,
  represents a string of bits in that base"
  [base-keyword s]
    (to-str [_]
      (case base-keyword
        :binary (str "B\"" s "\"")
        :octal  (str "O\"" s "\"")
        :hex    (str "X\"" s "\"")
        (do (println base-keyword "is not a valid base for a bit-string") ("\"0\"")))))

(defn binary-bit-string [s] (bit-string :binary s))
(defn octal-bit-string [s] (bit-string :octal s))
(defn hex-bit-string [s] (bit-string :hex s))

(proto/defalpha-item bit
  "Given either a 0 or 1 (as int or string) represents just that bit"
  [b]
    (to-str [_] (str "'" b "'")))

(proto/defalpha-item slog
  "Given either a 0 or 1 (as int or string) represents just that bit as a
  std_logic"
  [b]
    (to-str [_] (str "std_logic('" b "')")))

(proto/defalpha-item bit-vec
  "Given either a 0's or 1's (as a string) represents that bit vector"
  [b]
    (to-str [_] (str "\"" b "\"")))

(proto/defalpha-item slog-vec
  "Given either a 0's or 1's (as a string) represents that std_logic_vector"
  [b]
    (to-str [_] (str "std_logic_vector(\"" b "\")")))

(proto/defalpha-item bool
  "Given a boolean, represents that value"
  [v]
  (to-str [_] (if v "TRUE" "FALSE")))

(comment [
  (def r (raw :ohai))
  (proto/to-str r)
  (def n (numeric 10 23))
  (proto/to-str n)
  (def n2 (num2 "0010"))
  (proto/to-str n2)
  (def s (string "he said \"she said\""))
  (proto/to-str s)
  (def bb (bit-string :binary "001"))
  (proto/to-str bb)
  (def bo (bit-string :octal "755"))
  (proto/to-str bo)
  (def bh (bit-string :hex "ABC123"))
  (proto/to-str bh)
  (def b (bool true))
  (proto/to-str b)
])
