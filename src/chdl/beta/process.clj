(ns chdl.beta.process
  (:require 
            [chdl.beta.comp :refer [do-statements]]
            [chdl.beta.contracts :as contracts]
            [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as p]))

(defn process 
  "Create a process given a seq of signal names as the sensitivity-list.
   Optional declarations to create signals and variables.
   and finally the body of code inside the process statement as a seq"
  [{:keys [sensitivity-list declarations label body] 
    :or {declarations [] sensitivity-list []} :as args}]
  {:pre [(contracts/hashmap-has? args [:body])]}
  (expr/concated
    (expr/newlined
      (expr/concated
        (if-not (nil? label) (expr/concated label (lit/raw ": ")) (lit/raw ""))
        (lit/raw :process)
        (if-not (empty? sensitivity-list)
          (expr/parend
            (apply expr/comma-sepd sensitivity-list))
          (lit/raw ""))))
    (apply do-statements declarations)
    (expr/newlined (lit/raw :begin))
    (apply do-statements body)
    (expr/semicolond (lit/raw "end process"))))
