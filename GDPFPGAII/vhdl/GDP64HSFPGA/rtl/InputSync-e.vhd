-- -----------------------------------------------
-- Title:    Synchronisierung eines async. Inputs, 2- Stufig
-- file:     dff.vhd
-- language: VHDL 93
-- author:       HSSE / Andreas Voggeneder
-- comments: 
-- history: 
--   11.2001 creation
-- -----------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
--use work.DffGlobal.all;

entity InputSync is
  generic(levels_g     : natural :=2;
          ResetValue_g : std_ulogic := '0');
  port(Input     : in  std_ulogic;
       clk   : in  std_ulogic;
       clr_n : in  std_ulogic;
       q     : out std_ulogic);
end InputSync;




