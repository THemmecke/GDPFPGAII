--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - global package
-- File        : gdp_global-p.vhd
-- Description :
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2007 by Andreas Voggeneder
-----------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package gdp_global is
  
  constant nr_mon_sigs_c   : natural := 16;
  constant color_support_c : boolean :=true; 
  constant use_clut_c      : boolean :=true;
  
  -- Decoder
  constant commandWidth_c    : natural := 4;  -- 4 bit Opcode, 1 bit IRQ Enable, 1 bit Valid
  constant posWidth_c        : natural := 12; -- Bitwidth of Coordinates
  constant deltaWidth_c      : natural := 10; -- signed +/- 0x100
  constant lineStyleWidth_c  : natural := 2;  -- Bitwidth of Lines

  constant SizeOfOpcode_c    : natural                                      := 4;
  constant cmd_WPEN_c        : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0000";
  constant cmd_RPEN_c        : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0001";
  constant cmd_PEN_DOWN_c    : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0010";
  constant cmd_PEN_UP_c      : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0011";
  constant cmd_CLRS_c        : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0100";
  constant cmd_CLRXY_c       : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0101";
  constant cmd_CLRS_CLRXY_c  : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0110";
  constant cmd_CLRS_CLRALL_c : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "0111";
  constant cmd_PEN_WHITE_c   : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1000";
  constant cmd_PEN_c         : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1001";
  constant cmd_DRAW5x8_c     : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1010";
  constant cmd_DRAW4x4_c     : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1011";
  constant cmd_INVERS_c      : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1100";
  constant cmd_CLRX_c        : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1101";
  constant cmd_CLRY_c        : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1110";
  constant cmd_DMA_c         : std_ulogic_vector(SizeOfOpcode_c-1 downto 0) := "1111";


  subtype coord_t        is std_ulogic_vector(posWidth_c-1 downto 0);
  subtype delta_t        is std_ulogic_vector(deltaWidth_c-1 downto 0); -- 1 bit sign, 8 bit mantisse
  subtype lineStyle_t    is std_ulogic_vector(1 downto 0);
  subtype symbol_t       is std_ulogic_vector(7 downto 0);
  subtype symbolStyle_t  is std_ulogic_vector(1 downto 0);
  subtype scale_t        is std_ulogic_vector(3 downto 0);
  
  type drawCmd_t is (idle_e, clearScreen_e, drawLine_e, drawSymbol_e, DMA_e);

  type lineStyles_t is array (natural range <>) of std_ulogic_vector(0 to 7);
  constant lineStyles_c : lineStyles_t(0 to 3) :=
  (
    0 => "11111111", -- solid
    1 => "10101010", -- dotted
    2 => "11001100", -- dashed
    3 => "11111010"  -- dashdot
  );


  
end package gdp_global;
