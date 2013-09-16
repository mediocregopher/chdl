(ns chdl.alpha.expr
  (:require [chdl.alpha.proto :as proto]
            [chdl.alpha.literal :as literal]))

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

(proto/defalpha-item concated
  "Given one or more alpha items, represents their forms joined with nothing
  in-between"
  [items]
    (to-str [_]
      (apply str (map proto/to-str items))))

(proto/to-str (concated [(literal/raw :a) (literal/raw :b)]))

(defn parend [item]
  (concated [
    (literal/raw \()
    item
    (literal/raw \)) ]))

(comment
  (def s (space-sep [ (literal/raw :one) (literal/raw :two) (literal/raw :three)]))
  (proto/to-str s)

  (def c (comma-sep [ (literal/raw :one) (literal/raw :two) (literal/raw :three)]))
  (proto/to-str c)

  (def sc (semicolond c))
  (proto/to-str sc)

  (def cc (commad s))
  (proto/to-str cc)

  (def nc (newlined c))
  (proto/to-str nc)

  (def tc (tabd s))
  (proto/to-str tc)

  (def tc (concated [s c (semicolond c)]))
  (proto/to-str tc)

  (def pc (parend c))
  (proto/to-str pc)

  (proto/to-str (tabd (newlined (semicolond s))))
)
