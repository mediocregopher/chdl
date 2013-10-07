(ns chdl.alpha.alpha-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [chdl.alpha.proto :as proto]
            [chdl.alpha.expr :refer :all]
            [chdl.alpha.literal :refer :all]))

(facts "About the alpha layer"
  (fact "Checking the literals"
    (proto/to-str (raw :ohai))                      => "ohai"
    (proto/to-str (numeric 10 23))                  => "10#23#"
    (proto/to-str (num2 "0010"))                    => "2#0010#"
    (proto/to-str (string "he said \"she said\""))  => "\"he said \"\"she said\"\"\""
    (proto/to-str (bit-string :binary "001"))       => "B\"001\""
    (proto/to-str (bit-string :octal "755"))        => "O\"755\""
    (proto/to-str (bit-string :hex "ABC123"))       => "X\"ABC123\""
    (proto/to-str (bool true))                      => "TRUE"
    (proto/to-str (bool false))                     => "FALSE")

  (fact "Testing expressions!"
    (proto/to-str (concated (raw :a) (raw :b))) => "ab"
    (let [s  (space-sepd (raw :one) (raw :two) (raw :three))
          c  (comma-sepd (raw :one) (raw :two) (raw :three))
          sc (semicolond c)
          cc (commad s)
          nc (newlined c)
          tc (tabd s)
          ttc (tabd (concated nc nc))
          pc (parend c)
          combo (tabd (newlined (semicolond s)))]
      (proto/to-str s)     => "one two three"
      (proto/to-str c)     => "one, two, three"
      (proto/to-str sc)    => "one, two, three;"
      (proto/to-str cc)    => "one two three,"
      (proto/to-str nc)    => "one, two, three\n"
      (proto/to-str tc)    => "    one two three"
      (proto/to-str ttc)   => "    one, two, three\n    one, two, three\n"
      (proto/to-str pc)    => "(one, two, three)"
      (proto/to-str combo) => "    one two three;\n")))
