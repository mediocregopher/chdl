(ns chdl.beta.beta-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [chdl.alpha.literal :as lit :refer :all]
            [chdl.beta.process :refer :all]
            [chdl.alpha.proto :as proto]
            [chdl.beta.comp :refer :all]
            [clojure.string :as s]))

(defn clean-spaces [string]
  (s/replace string #"\s+" " ")) 

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
      "USE IEEE.STD_LOGIC_1164, HARDI.Devices.All")

  (fact "A simple process statement"
    (clean-spaces
      (proto/to-str
        (process {:body []})))  

    => (clean-spaces "process 
                      begin
                      end process;")) 

  (fact "A more complex process statement"
    (clean-spaces 
      (proto/to-str 
        (process {:declarations [ (variable :l :line) ]
                  :body [ 
                         (paren-call :write (raw :l) (paren-call :String' (string "Hello world!")))
                         (paren-call :writeline (raw :output) (raw :l))
                         (raw :wait)]})))

    => (clean-spaces 
         "process
             VARIABLE l : line;
          begin
             write(l, String'(\"Hello world!\"));
             writeline(output, l);
             wait;
          end process;"))

)
