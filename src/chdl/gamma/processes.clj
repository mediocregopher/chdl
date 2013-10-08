(ns chdl.gamma.processes
  (:require [chdl.alpha.literal :as lit]
            [chdl.beta.math :as m]
            [chdl.beta.comp :as c]
            [chdl.beta.process :as proc]
            [chdl.alpha.expr :as expr]
            [chdl.alpha.proto :as proto]))
(comment 

;; Ideally, I'd like to do something like this:

  (let-proc [some-variable (variable (bit 0))
             some-sig (signal (bit 0)) 
             :watch clk rst]
    (cond 
      (= rst (bit 1)) (assign-signal! state (bit-vec "0000"))
      (= load-n (bit 1)) (assign-signal! state (unsigned input))
      (and (= clk (bit 1)) (event? clk)) 
        (cond
          (= up-n (bit 0)) (assign-signal! state (inc state))
          (= up-n (bit 1)) (assign-signal! state (dec state)))))

    process(clk, rst)
    begin
            if(rst = '1') then
                state <= "0000";
            elsif(load_n = '0') then
                state <= unsigned(input);
            elsif (clk='1' and clk'event) then
                if (up_n = '0') then
                    -- go up
                    state <= state + 1;
                elsif(up_n = '1') then
                    state <= state - 1;
                end if;
            end if;
    end process;



)
