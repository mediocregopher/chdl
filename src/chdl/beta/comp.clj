(ns chdl.beta.comp
  (:require [chdl.alpha.literal :as lit]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]))

(defn paren-call
  "Given a function call (or whatever, it's vhdl) and arguments, represents the
  'func(arg1,arg2,arg3)' syntax"
  [func & args]
  (expr/concated
    (lit/raw func)
    (expr/parend (apply expr/comma-sepd args))))

(defn downto
  "Given two numeric literals a and b, represents the 'a downto b' syntax"
  [a b]
  (expr/space-sepd a (lit/raw :DOWNTO) b))

(defn array-of
  "Given an array type (for example, :bit) and at least one downto statement
  (more than one for a multi-dimensional array), represents the 'array(a downto
  b) of :type' syntax"
  [typ dt0 & dt]
  (expr/space-sepd
    (apply paren-call :ARRAY dt0 dt)
    (lit/raw :OF)
    (lit/raw typ)))

(defn array
  "Given one or more literals (or other array expressions, as would be used with
  a multi-level array), represents the '(\"00\",\"01\",\"10\")' syntax"
  [e0 & e]
  (expr/parend (apply expr/comma-sepd e0 e)))

(defn others
  "Given some literal (or possibly another others expression, as would be used
  with a multi-level array), represents the '(OTHERS => :somebit)' syntax"
  [b]
  (expr/parend (expr/space-sepd
    (lit/raw :OTHERS)
    (lit/raw :=>) ;lol
    b)))

(defn- sigcon
  "Given a constant identifier (string or symbol), it's type, and an optional
  default, represents the 'CONSTANT id : type [:= default]' syntax"
  [sigcon id typ & default]
  (apply expr/space-sepd
    (lit/raw sigcon)
    (lit/raw id)
    (lit/raw \:)
    (lit/raw typ)
    (if-not (empty? default)
      (cons (lit/raw ":=") default) '())))

(def constant (partial sigcon :CONSTANT))
(def signal (partial sigcon :SIGNAL))

(defn assign
  "Given a signal id and what it should be assigned to, represents the
  'id <= towhat' syntax"
  [id towhat]
  (expr/space-sepd
    (lit/raw id)
    (lit/raw "<=")
    towhat))

(defn port
  "Given partitions of three arguments, each set having a signal identifier, the
  direction of the signal (:in, :out, or :inout), and the signal's type,
  represents the PORT(id : direction type; ...) syntax"
  [& args]
  (expr/concated (lit/raw :PORT) (expr/parend
    (apply expr/sepd (lit/raw ";\n")
      (map (fn [argset]
        (let [[id inout typ] argset]
          (expr/space-sepd
            (lit/raw id)
            (lit/raw \:)
            (lit/raw inout)
            (lit/raw typ))))
        (partition 3 args))))))

(defn- lib-loaduse
  "Given one or more libraries that need to be imported, represents the
  'LIBRARY/USE lib1, lib2' syntax"
  [libuse & libs]
  (expr/space-sepd
    (lit/raw libuse)
    (apply expr/comma-sepd (map lit/raw libs))))

(def lib-load (partial lib-loaduse :LIBRARY))
(def lib-use (partial lib-loaduse :USE))

(defn concat
  "Given one or more arrays/elements, represents the 'a & b & c ...'
  concatanation syntax"
  [& args]
  (apply expr/sepd (lit/raw " & ") args))

(comment [
  (proto/to-str (paren-call :func (lit/string "ohai") (lit/num2 "001")))
  (proto/to-str (downto (lit/num10 8) (lit/num2 "01")))
  (proto/to-str (array-of :BIT
    (downto (lit/num10 7) (lit/num10 0))
    (downto (lit/num10 7) (lit/num10 0))))
  (proto/to-str (array (lit/bit 0) (lit/bit 1)))
  (proto/to-str (array
    (array (lit/bit 0) (lit/bit 1))
    (array (lit/bit 0) (lit/bit 1))))
  (proto/to-str (others (lit/bit 0)))
  (proto/to-str (constant :a :REAL))
  (proto/to-str (constant :a :REAL (lit/num10 25)))
  (proto/to-str (signal :a :REAL))
  (proto/to-str (signal :a :REAL (lit/num10 25)))
  (proto/to-str (assign :a (lit/num2 "1001")))
  (proto/to-str (port :sig1 :in :real
                      :sig2 :out :real))
  (proto/to-str (lib-load "IEEE" "HARDI"))
  (proto/to-str (lib-use "IEEE.STD_LOGIC_1164" "HARDI.Devices.All"))
  (proto/to-str (concat (lit/bit 0) (lit/bit 1)))
])
