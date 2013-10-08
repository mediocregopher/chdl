(ns chdl.beta.process
  (:require [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as p]))

(defn do-statements 
  "Given statements, return a new literal that combines them all 
  with a semicolon and newline at the end of every statement"
  [& statements] 
  (apply expr/concated
    (for [s statements]
      (expr/newlined (expr/semicolond s)))))

(defn process 
  "Create a process given a seq of signal names as the sensitivity-list.
   Optional declarations to create signals and variables.
   and finally the body of code inside the process statement as a seq"
  [{:keys [sensitivity-list declarations label body] 
    :or {declarations [] sensitivity-list []}}]
  (expr/concated
    (expr/newlined
      (expr/concated
        (if-not (nil? label) (expr/concated (lit/raw label) (lit/raw ": ")) (lit/raw ""))
        (lit/raw :process)
        (if-not (empty? sensitivity-list)
          (expr/parend
            (apply expr/comma-sepd sensitivity-list))
          (lit/raw ""))))
    (apply do-statements declarations)
    (expr/newlined (lit/raw :begin))
    (apply do-statements body)
    (expr/semicolond (lit/raw "end process"))))
