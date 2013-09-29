(ns chdl.beta.beta-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [chdl.alpha.literal :refer :all]
            [chdl.alpha.proto :as proto]
            [chdl.beta.comp :refer :all]))

(facts "About the beta layer"

  (fact "Checking the components"
    (proto/to-str
      (paren-call :func
        (string "ohai")
        (num2 "001"))) => "func(\"ohai\", 2#001#)"

    (proto/to-str
      (downto (num10 8) (num2 "01"))) => "10#8# DOWNTO 2#01#"

    (proto/to-str (array-of :BIT
      (downto (num10 7) (num10 0))
      (downto (num10 7) (num10 0)))) =>
        "ARRAY(10#7# DOWNTO 10#0#, 10#7# DOWNTO 10#0#) OF BIT"

    (proto/to-str (array (bit 0) (bit 1))) => "('0', '1')"

    (proto/to-str (array
      (array (bit 0) (bit 1))
      (array (bit 0) (bit 1)))) => "(('0', '1'), ('0', '1'))"

    (proto/to-str (others (bit 0))) => "(OTHERS => '0')"


    (proto/to-str (constant :a :REAL)) =>
      "CONSTANT a : REAL"

    (proto/to-str (constant :a :REAL (num10 25))) =>
      "CONSTANT a : REAL := 10#25#"

    (proto/to-str (signal :a :REAL)) =>
      "SIGNAL a : REAL"

    (proto/to-str (signal :a :REAL (num10 25))) =>
      "SIGNAL a : REAL := 10#25#"

    (proto/to-str (port :sig1 :in :real
                        :sig2 :out :real)) =>
      "PORT(sig1 : in real;\nsig2 : out real)"

    (proto/to-str (lib-load "IEEE" "HARDI")) => "LIBRARY IEEE, HARDI"

    (proto/to-str (lib-use "IEEE.STD_LOGIC_1164" "HARDI.Devices.All")) =>
      "USE IEEE.STD_LOGIC_1164, HARDI.Devices.All"
))
