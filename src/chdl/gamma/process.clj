(ns chdl.gamma.process
  (:require 
    [chdl.gamma.types :as t]
    [chdl.alpha.literal :as lit]
    [chdl.beta.math :as m]
    [chdl.beta.comp :as c]
    [chdl.beta.process :as proc]
    [chdl.alpha.expr :as expr]
    [chdl.alpha.proto :as proto]
    [chdl.gamma.protocols :as gamma-proto]))
(comment 

;; Ideally, I'd like to do something like this:

  (let-proc [some-variable (variable (bit 0))
             some-sig (signal (bit 0)) 
             :watch clk rst]
    (cond 
      (high? rst)                                 (assign-signal! state (bit-vec "0000"))
      (low? load-n)                               (assign-signal! state (unsigned input))
      (and (high? clk) (event? clk) (low? up-n))  (assign-signal! state (inc state))
      (and (high? clk) (event? clk) (high? up-n)) (assign-signal! state (dec state))))


;    process(clk, rst)
;      signal some_sig : '0';
;      variable some_variable : '0';
;    begin
;            if(rst = '1') then
;                state <= "0000";
;            elsif(load_n = '0') then
;                state <= unsigned(input);
;            elsif (clk='1' and clk'event) then
;                if (up_n = '0') then
;                    state <= state + 1;
;                elsif(up_n = '1') then
;                    state <= state - 1;
;                end if;
;            end if;
;    end process;

)

(defmacro let-proc [bindings & body]
  (let [[bindings sensitivity] (split-with (partial not= :watch) bindings)
        sensitivity (vec (rest sensitivity))
        symbols (vec (take-nth 2 bindings))]
    `(proc/process
      (let ~(vec bindings)
         {:sensitivity-list ~sensitivity
          :declarations (map gamma-proto/construct ~symbols)
          :body ~(vec body)}))))
    

(comment 


(def clk (t/signal (t/bit 0)))
(def rst (t/signal (t/bit 0)))


(println 
  (proto/to-str
  (let-proc
    [some-variable (t/variable (t/bit 0))
     some-sig (t/signal (t/bit 0))
     :watch clk rst]
    (c/paren-call :write some-variable (c/paren-call :String' (lit/string "Hello world!"))))))
)
