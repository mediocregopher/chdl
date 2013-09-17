(ns chdl.alpha.proto)

(defprotocol alpha-item
  (to-str [x]))

(defmacro defalpha-item [item-name doc args & body]
  (let [item-type (gensym item-name)]
    `(do
      (deftype ~item-type ~args alpha-item ~@body)
      (defn ~item-name ~doc ~args (new ~item-type ~@args)))))

(comment 
  ;; Demonstrate the macro's functionality
  (macroexpand  
    '(defalpha-item numeric
      "Given a base and a number in that base as a string, represents that number"
      [base n]
        (to-str [_] (str base "#" n "#"))))
  )

