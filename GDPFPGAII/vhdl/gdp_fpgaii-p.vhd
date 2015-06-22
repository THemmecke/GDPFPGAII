--------------------------------------------------------------------------------
-- Project     : GDPFPGAII
-- Module      : GDP 936X Display processor and Peripherals - global package / definitions
-- File        : gdp_fpgaii-p.vhd
-- Description : http://www.armadeus.com/wiki/index.php?title=A_simple_design_with_Wishbone_bus
--               http://www.eetimes.com/document.asp?doc_id=1278512
--------------------------------------------------------------------------------
-- Author       : Torsten Hemmecke
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2013 by Torsten Hemmecke
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package gdp_fpgaii_global is

constant is_68000_c		: boolean := true;

constant num_cs_signals_c : integer := 17; -- number of cs_signals generated in wrapper module an routed to other modules
constant use_cs_signals_c : boolean := true;

end package gdp_fpgaii_global;