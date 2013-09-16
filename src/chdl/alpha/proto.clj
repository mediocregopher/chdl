(ns chdl.alpha.proto)

(defprotocol alpha-item
  (to-str [x]))

(defmacro defalpha-item [item-name doc args & body]
  (let [item-name-t   (symbol (str (name item-name) "-t"))
        item-name-t-dot (symbol (str (name item-name) "-t."))]
    `(do
      (deftype ~item-name-t ~args alpha-item ~@body)
      (defn ~item-name ~doc ~args ~(cons item-name-t-dot args)))))
