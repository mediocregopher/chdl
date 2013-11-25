(ns chdl.beta.control
  "Control structures for vhdl"
  (:require 
    [chdl.gamma.types :as t]
    [chdl.alpha.literal :as lit]
    [chdl.beta.math :as m]
    [chdl.beta.comp :as c]
    [chdl.beta.process :as proc]
    [chdl.alpha.expr :as expr]
    [chdl.alpha.proto :as proto]))

(defn cond
  "Imitates the lisp cond, but on vhdl"
  [& options]
  {:pre (even? (count options))}
  (let [options (partition 2 options)]
    (expr/concated
      (reduce
        (fn [acc [predicate clauses]]
          (expr/concated
            acc
            (if (not= predicate :else)
              (expr/concated
               (expr/newlined
                 (expr/space-sepd
                   (lit/raw "elsif")
                   (expr/parend predicate)
                   (lit/raw "then")))
               (expr/newlined
                 (expr/tabd (expr/semicolond clauses))))
              (expr/concated
               (expr/newlined
                 (lit/raw "else"))
               (expr/newlined
                 (expr/tabd (expr/semicolond clauses)))))))
        (expr/concated
          (expr/newlined
            (expr/space-sepd
              (lit/raw "if")
              (expr/parend (first (first options)))
              (lit/raw "then")))
          (expr/newlined
            (expr/tabd (expr/semicolond (second (first options))))))
        (drop 1 options))
        (expr/newlined (expr/semicolond (lit/raw "end if"))))))

(comment
  (println
    (proto/to-str
      (cond
        (lit/raw "foo") (lit/raw "something")
        (lit/raw "foo1") (lit/raw "something1")
        (lit/raw "foo2") (lit/raw "something2")
        :else  (lit/raw "something2")
        )))
)
