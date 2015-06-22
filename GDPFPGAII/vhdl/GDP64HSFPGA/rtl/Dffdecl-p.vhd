-- -----------------------------------------------
-- Title:    Constants for DFF
-- file:     DffGlobal.vhd
-- language: VHDL 93
-- author:       HSSE / Andreas Voggeneder
-- comments: Package declaration
-- history: 
--   11.2001 creation
-- -----------------------------------------------


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package DffGlobal is
  constant activated_cn   : std_ulogic := '0';
  constant inactivated_cn : std_ulogic := '1';
  constant activated_c   : std_ulogic := '1';
  constant inactivated_c : std_ulogic := '0';
  constant ResetActive_c : std_ulogic := '0';
end package DffGlobal;








