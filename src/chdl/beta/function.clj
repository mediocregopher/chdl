(ns chdl.beta.function
  "The VHDL function emmitter"
  (:require 
            [chdl.beta.comp :refer [do-statements]]
            [chdl.beta.contracts :as contracts]
            [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as p]))

(defn function 
  "Takes in a function name, a seq of typed parameters, 
  a return type, and a body of sequential statements to create a
  VHDL function"
  [{:keys [name parameters declarations body return-type] 
    :or {declarations [] parameters []} :as args}]
  ;; Check to make sure that the function is given at least a name and body
  {:pre [(contracts/hashmap-has? args [:name :body :return-type])]}
  (expr/concated
    (expr/newlined
      (expr/space-sepd
        (lit/raw :function)
        name
        (if-not (empty? parameters)
          (expr/parend 
            (apply expr/comma-sepd parameters))
          (lit/raw ""))
        (lit/raw :return)
        return-type
        (lit/raw :is)))
    (apply do-statements declarations)
    (expr/newlined (lit/raw :begin))
    (apply do-statements body)
    (expr/semicolond 
      (expr/space-sepd
        (lit/raw :end)
        name))))

(comment
  (println
    (p/to-str (function {:name (lit/raw "foobar") :body [] :return-type (lit/raw "bit-vector")}))
    )

  (p/to-str
    (function {:name (lit/raw "Transcod_1") :parameters [lit/raw "Value: in bit_vector(0 to 7)"]
               :return-type (lit/raw "bit_vector")
               :body []}))

  )
