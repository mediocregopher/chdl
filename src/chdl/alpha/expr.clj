(ns chdl.alpha.expr
  (:require [chdl.alpha.proto :as proto]
            [chdl.alpha.literal :as literal]))

;; All functions in here take in literals from chdl.alpha.literal ;;

(proto/defalpha-item concated-raw
  "Given one or more alpha items, represents their forms joined with nothing
  in-between"
  [items]
    (to-str [_]
      (apply str (map proto/to-str items))))

;defalpha-item is a macro and doesn't handle & correctly, so we wrap
;concated-raw in a defn
(defn concated [& items] (concated-raw items))

(defn semicolond [item] (concated item (literal/raw \;)))
(defn commad [item] (concated item (literal/raw \,)))
(defn newlined [item] (concated item (literal/raw \newline)))
(defn tabd [item] (concated (literal/raw "    ") item))
(defn parend [item] (concated (literal/raw \() item (literal/raw \))))

(defn sepd [sep & items] (concated-raw (interpose sep items)))
(defn space-sepd [& items] (apply sepd (literal/raw " ") items))
(defn comma-sepd [& items] (apply sepd (literal/raw ", ") items))

(comment
  (def tc (concated (literal/raw :a) (literal/raw :b)))
  (proto/to-str tc)

  (def s (space-sepd (literal/raw :one) (literal/raw :two) (literal/raw :three)))
  (proto/to-str s)

  (def c (comma-sepd (literal/raw :one) (literal/raw :two) (literal/raw :three)))
  (proto/to-str c)

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
