--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - Color Lookup Table
-- File        : GDP_clut.vhd
-- Description :
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2007 by Andreas Voggeneder
--------------------------------------------------------------------------------

library IEEE;

use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.DffGlobal.all;
use work.gdp_global.all;

entity gdp_clut is
  port (
      reset_n_i    : in  std_ulogic;
      clk_i        : in  std_ulogic; 
      clk_en_i     : in  std_ulogic;
      WrAddress_i  : in  std_ulogic_vector(3 downto 0); 
      Data_i       : in  std_ulogic_vector(8 downto 0); 
      WE_i         : in  std_ulogic; 
      RdAddress_i  : in  std_ulogic_vector(3 downto 0); 
      Data_o       : out std_ulogic_vector(8 downto 0));
end gdp_clut;

architecture rtl of gdp_clut is
  type CLUT_ARRAY is array(0 to 15) of std_ulogic_vector(8 downto 0);
  signal clut : CLUT_ARRAY:= ("000000000",
                             "111111111",
                             "111111000",
                             "000111000",
                             "111000000",
                             "000000111",
                             "111000111",
                             "000111111",
                             "001001001",
                             "100100100",
                             "011011000",
                             "000011000",
                             "011000000",
                             "000000011",
                             "011000011",
                             "000011011");
                              
begin
  process (reset_n_i,clk_i)
	begin
  	if reset_n_i = ResetActive_c then
      clut <= ("000000000",  -- "0 0000 0000"
               "111111111",  -- "0 0100 1001"
               "111111000",  -- "0 1001 0010"
               "000111000",  -- "0 1101 1011"
               "111000000",  -- "1 0010 0100"
               "000000111",  -- "1 0110 1101"
               "111000111",  -- "1 1011 0110"
               "000111111",  -- "1 1111 1111"
               "001001001",  -- "0 0000 0000"
               "100100100",  -- "0 0100 1001"
               "011011000",  -- "0 1001 0010"
               "000011000",  -- "0 1101 1011"
               "011000000",  -- "1 0010 0100"
               "000000011",  -- "1 0110 1101"
               "011000011",  -- "1 1011 0110"
               "000011011"); -- "1 1111 1111"
		elsif clk_i'event and clk_i = '1' then
		  if clk_en_i = '1' then
  			if WE_i = '1' then
  				clut(to_integer(unsigned(WrAddress_i))) <= Data_i;
  			end if;
---- pragma translate_off
--			if not is_x(RdAddress_i) then
---- pragma translate_on
--	      Data_o <= clut(to_integer(unsigned(RdAddress_i)));
---- pragma translate_off
--      else
--        Data_o <= (others =>'-');
--			end if;
---- pragma translate_on
  	  end if;
		end if;
	end process;
	
	process(clut,RdAddress_i)
	begin
-- pragma translate_off
			if not is_x(RdAddress_i) then
-- pragma translate_on
	      Data_o <= clut(to_integer(unsigned(RdAddress_i)));
-- pragma translate_off
      else
        Data_o <= (others =>'-');
			end if;
-- pragma translate_on
  end process;
end rtl;
