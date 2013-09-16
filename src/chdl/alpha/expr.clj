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

(proto/defalpha-item concated
  "Given one or more alpha items, represents their forms joined with nothing
  in-between"
  [items]
    (to-str [_]
      (apply str (map proto/to-str items))))

(defn semicolond [item] (concated [item (literal/raw \;)]))
(defn commad [item] (concated [item (literal/raw \,)]))
(defn newlined [item] (concated [item (literal/raw \newline)]))
(defn tabd [item] (concated [(literal/raw "    ") item]))
(defn parend [item] (concated [ (literal/raw \() item (literal/raw \)) ]))

(comment
  (def s (space-sep [ (literal/raw :one) (literal/raw :two) (literal/raw :three)]))
  (proto/to-str s)

  (def c (comma-sep [ (literal/raw :one) (literal/raw :two) (literal/raw :three)]))
  (proto/to-str c)

  (def tc (concated [s c]))
  (proto/to-str tc)

  (def sc (semicolond c))
  (proto/to-str sc)

  (def cc (commad s))
  (proto/to-str cc)

  (def nc (newlined c))
  (proto/to-str nc)

  (def tc (tabd s))
  (proto/to-str tc)

  (def pc (parend c))
  (proto/to-str pc)

  (proto/to-str (tabd (newlined (semicolond s))))
)
