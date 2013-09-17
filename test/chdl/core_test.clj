(ns chdl.core-test
  (:use midje.sweet)
  (:require [clojure.test :refer :all]
            [chdl.core :refer :all]))

(facts "Test if this is awesome"
  (fact "This is awesome"
    (= :awesome :awesome) => true))

