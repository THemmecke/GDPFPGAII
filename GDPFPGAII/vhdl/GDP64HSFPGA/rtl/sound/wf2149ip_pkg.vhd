--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : Soundgenerator - Package
-- File        : WF2149ip_pkg.vhd
-- Description : YM2149 / AY-3-891X compatible sound generator.
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2007 by Andreas Voggeneder
--------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

package WF2149IP_PKG is
type BUSCYCLES_t is (INACTIVE, READ, WRITE, ADDRESS);

  component WF2149IP_WAVE
  	port(
  		RESETn		: in std_ulogic;
  		SYS_CLK		: in std_ulogic;
  
  		WAV_STRB	: in std_ulogic;
  
  		ADR			: in std_ulogic_vector(3 downto 0);
  		DATA_IN		: in std_ulogic_vector(7 downto 0);
  		DATA_OUT	: out std_ulogic_vector(7 downto 0);
  		DATA_EN		: out std_ulogic;
  		
  		BUSCYCLE	: in BUSCYCLES_t;
  		CTRL_REG	: in std_ulogic_vector(5 downto 0);
  
--  		OUT_A		: out std_ulogic;
--  		OUT_B		: out std_ulogic;
--  		OUT_C		: out std_ulogic
      OUT_SUM     : out std_ulogic
  	);
  end component;
  
  component dac
    generic (
      msbi_g : integer := 7
    );
    port (
      clk_i   : in  std_ulogic;
      res_n_i : in  std_ulogic;
      dac_i   : in  std_ulogic_vector(msbi_g downto 0);
      dac_o   : out std_ulogic
    );
  end component;
end WF2149IP_PKG;