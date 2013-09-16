(ns chdl.alpha.expr
  (:require [chdl.alpha.proto :as proto]
            [chdl.alpha.literal :as literal]))

(proto/defalpha-item enum
  "Given a sequence of character or raw literals, represents an enumeration of
  those literals"
  [items]
    (to-str [_]
      (let [item-strs (map proto/to-str items)
            item-full-str (apply str (interpose "," item-strs))]
        (str "(" item-full-str ")"))))

(proto/defalpha-item space-sep
  "Given a sequence of alpha-items, represents their space-separated form"
  [items]
    (to-str [_]
      (apply str (interpose " " (map proto/to-str items)))))


(proto/defalpha-item comma-sep
  "Given a sequence of alpha-items, represents their comma-separated form"
  [items]
    (to-str [_]
      (apply str (interpose ", " (map proto/to-str items)))))

(proto/defalpha-item appended
  "Given an alpha-item, represents it but with something trailing it"
  [item ch]
    (to-str [_]
      (str (proto/to-str item) ch)))

(defn semicolond [item] (appended item \;))
(defn commad [item] (appended item \,))
(defn newlined [item] (appended item \newline))

(proto/defalpha-item prepended
  "Given an alpha-item, represents it but with something preceding it"
  [item ch]
    (to-str [_]
      (str ch (proto/to-str item))))

(defn tabd [item] (prepended item "    "))

(comment
  (def e (enum [ (literal/raw :one) (literal/raw "two") (literal/character \a)]))
  (proto/to-str e)
  (def s (space-sep [ (literal/raw :one) (literal/raw :two) (literal/raw :three)]))
  (proto/to-str s)
  (def c (comma-sep [ (literal/raw :one) (literal/raw :two) (literal/raw :three)]))
  (proto/to-str c)
  (def sc (semicolond c))
  (proto/to-str sc)
  (def cc (commad s))
  (proto/to-str cc)
  (def nc (newlined e))
  (proto/to-str nc)
  (def tc (tabd s))
  (proto/to-str tc)
  (proto/to-str (tabd (newlined (semicolond s))))
)
