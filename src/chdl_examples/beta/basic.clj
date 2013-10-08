(ns chdl-examples.beta.basic
  (:require [chdl.alpha.literal :as lit]
            [chdl.alpha.proto :as proto]
            [chdl.beta.comp :as comp]
            [chdl.beta.math :as math]
            [chdl.beta.design :as design]))

(def numsigs 8)
(def in-sigs (map #(str "inSig" %1) (range numsigs)))
(def out-sigs (map #(str "outSig" %1) (range (/ numsigs 2))))

(def ports 
  (apply 
    comp/port 
    (concat
      (apply concat (map #(vector %1 :in :BIT) in-sigs))
      (apply concat (map #(vector %1 :out :BIT) out-sigs)))))

(def assigns 
  (map
    #(let [[[in1 in2] out] %1]
      (comp/assign-signal! out (math/xor (lit/raw in1) (lit/raw in2))))
    (map vector (partition 2 in-sigs) out-sigs))) ; This is how clojure zips

(def ent (design/entity :xorer ports))
(def arch (design/architecture :xorer :arch [] assigns))

(defn -main [& args]
  (println (proto/to-str ent))
  (println (proto/to-str arch)))

(comment
  (-main)
)
