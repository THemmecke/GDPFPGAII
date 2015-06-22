--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : Businterface
-- File        : gdp_bi.vhd
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

entity gdp_bi is
  port(reset_n_i     : in  std_ulogic;
       clk_i         : in  std_ulogic;
       fpga_en_i     : in  std_ulogic; -- wird aus unsynchronisierten Signalen (IOREQ und ADDD) erzeugt, siehe gdp_lattice_top.vhd
       addr_o        : out std_ulogic_vector(7 downto 0);
       data_in_o     : out std_ulogic_vector(7 downto 0);
       IORQ_o        : out std_ulogic; -- ==> IORQ
       Rd_o          : out std_ulogic; -- ==> gdp_RD
       Wr_o          : out std_ulogic; -- ==> gdp_WR
       nRd_sync_o    : out std_ulogic; -- ==> nRD
       nWr_sync_o    : out std_ulogic; -- ==> nWR
       --nIORQ_sync_o  : out std_ulogic; -- ==> ? NC
       -------------------------------
       -- async inputs
       -------------------------------
       nkc_nIORQ_i   : in  std_ulogic;
       nkc_nRD_i     : in  std_ulogic;
       nkc_nWR_i     : in  std_ulogic;
       nkc_ADDR_i    : in  std_ulogic_vector(7 downto 0);
       nkc_DB        : in  std_logic_vector(7 downto 0)
  );
end gdp_bi;

architecture rtl of gdp_bi is
  component InputSync
    generic(levels_g     : natural :=1;
            ResetValue_g : std_ulogic := '0');
    port (
      Input : in  std_ulogic;
      clk   : in  std_ulogic;
      clr_n : in  std_ulogic;
      q     : out std_ulogic);
  end component;
  
  signal nIORQ,nIORQ_d     : std_ulogic;
  signal nRD,nRD_d         : std_ulogic;
  signal nWR,nWR_d         : std_ulogic;
begin

  ISIORQ : InputSync
  generic map (
    ResetValue_g => '1'
  )
  port map (
      Input => nkc_nIORQ_i,
      clk   => clk_i,
      clr_n => reset_n_i,
      q     => nIORQ);
  
  ISRD : InputSync
  generic map (
    ResetValue_g => '1'
  )
  port map (
      Input => nkc_nRD_i,
      clk   => clk_i,
      clr_n => reset_n_i,
      q     => nRD);
      
  ISWR : InputSync
  generic map (
    ResetValue_g => '1'
  )
  port map (
      Input => nkc_nWR_i,
      clk   => clk_i,
      clr_n => reset_n_i,
      q     => nWR);
   
  process(clk_i,reset_n_i)
  begin
    if reset_n_i = '0' then
      nIORQ_d      <= '1';
      nRD_d        <= '1';
      nWR_d        <= '1';
      Rd_o         <= '0';
      Wr_o         <= '0';
      addr_o       <= (others => '0');
      data_in_o    <= (others => '0');
--      output_en    <= '0';
    elsif rising_edge(clk_i) then
      nIORQ_d      <= nIORQ; -- for edge detection
      nWR_d        <= nWR;
      nRD_d        <= nRD;
--      output_en    <= gdp_cs or key_cs or dip_cs or mouse_cs or ser_cs or flo_addr;
--      output_en    <= fpga_en_i;
      
      Rd_o    <= '0';
      Wr_o    <= '0';
      if nIORQ = '0' then
        if nIORQ_d = '1' then
          -- IORQ  had an falling edge.
          -- Store Address
          addr_o <= nkc_ADDR_i;
        end if;
        if fpga_en_i = '1' or nRD = '0' then
--          nWR_d  <= nWR;
--          nRD_d  <= nRD;
          Rd_o   <= not nRD and nRD_d; 					-- Rd_o geht für einen Clock-Cycle auf 1 (fallende Flanke von nRD detektiert)
          if (not nWR and nWR_d)='1' then				-- detect falling edge in nWR
            data_in_o <= std_ulogic_vector(nkc_DB);     --   store data from databus
            Wr_o      <= '1';							--   and set Wr_o to 1 for 1 clock cycle
          end if;
        else
          nWR_d <= '1';
          nRD_d <= '1';
        end if;
      else
        nWR_d <= '1';
        nRD_d <= '1';
      end if;
    end if;
  end process;
  
  IORQ_o       <= (not nIORQ and not nIORQ_d);			-- set IOREQ_o 1 after 1 clock cycle
  --nIORQ_sync_o <= nIORQ;
  nRd_sync_o   <= nRd;
  nWr_sync_o   <= nWr;
end rtl;  
