(ns chdl.beta.contracts
  "A collection of helpful checks to aid in error reporting")

(defn hashmap-has? [hashmap keys] 
  (every? (partial contains? hashmap) keys))
