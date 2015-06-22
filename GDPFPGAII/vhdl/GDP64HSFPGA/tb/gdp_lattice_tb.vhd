-------------------------------------------------------------------------------
-- Title      : Testbench for design "gdp_kernel"
-- Project    :
-------------------------------------------------------------------------------
-- File       : gdp_kernel_tb.vhd
-- Author     :   <Andreas Voggeneder@LAPI>
-- Company    :
-- Created    : 2007-04-08
-- Last update: 2007-04-08
-- Platform   :
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2007
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2007-04-08  1.0      Andreas Voggeneder  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

-------------------------------------------------------------------------------

entity gdp_lattice_tb is

end gdp_lattice_tb;

-------------------------------------------------------------------------------

architecture beh of gdp_lattice_tb is

  -- clock
  signal Clk     : std_logic := '1';
  signal CPU_Clk : std_logic := '1';
  -- component ports
  signal reset_n_i : std_ulogic;
  signal nkc_DB_buf  : std_logic_vector(15 downto 0); 
  signal nkc_DB      : std_logic_vector(15 downto 0);
  signal nkc_ADDR    : std_ulogic_vector(19 downto 0);
  signal nkc_nRD     : std_ulogic;
  signal nkc_nWR     : std_ulogic;
  signal nkc_nIORQ   : std_ulogic;
  signal nkc_nMEMRQ  : std_ulogic;
  signal nkc_nWAIT 	 : std_ulogic;
  signal driver_nEN  : std_ulogic;
  signal driver_DIR  : std_ulogic;
  signal m68k_nAS,m68k_nRW,m68k_nUDS,m68k_nLDS : std_ulogic;

  --signal SRAM_addr_o    : std_logic_vector(17 downto 0);
--  signal SRAM_data_o    : std_ulogic_vector(7 downto 0);
--  signal SRAM_data_i    : std_ulogic_vector(7 downto 0);
--  signal SRAM_ena_o     : std_ulogic;
--  signal SRAM_we_o      : std_ulogic;
  signal SRAM_Addr                : std_logic_vector(17 downto 0);
  signal SRAM_DB                  : std_logic_vector(15 downto 0);
  signal SRAM_nCS0                : std_logic;
  signal SRAM_nCS1                : std_logic;
  signal SRAM_nWR                 : std_logic;
  signal SRAM_nOE                 : std_logic;
  signal SRAM_nBHE                : std_logic;
  signal SRAM_nBLE                : std_logic; 
--  signal SRAM1_Addr               : std_logic_vector(17 downto 0);  
--  signal SRAM1_nWR                : std_logic;
--  signal SRAM1_nOE                : std_logic;
--  signal SRAM1_nBHE               : std_logic;
--  signal SRAM1_nBLE               : std_logic;
  
  signal do_dump                   : boolean;
  signal Ps2Clk                    : std_logic;
  signal Ps2Dat                    : std_logic;
  signal Ps2MouseClk               : std_logic;
  signal Ps2MouseDat               : std_logic;
  signal TxD                       : std_ulogic;
                                   
  signal IDE_D	                   : std_logic_vector(15 downto 0);
  signal SD_MOSI                   : std_ulogic;
--  signal VDIP_SCK       : std_ulogic;
--  signal VDIP_CS        : std_ulogic;
--  signal VDIP_MOSI      : std_ulogic;
--  signal VDIP_MISO      : std_ulogic;
--  signal vdip_data      : std_ulogic_vector(7 downto 0);

begin  -- beh
   IDE_D <= (others => 'H');
  
  DUT: entity work.gdp_fpgaii_top
    generic map(sim_g => true)
    port map (
      reset_n_i   => reset_n_i,
--      addr_sel_i  => '1',
      clk_i       => clk,
      RxD_i       => TxD,
      TxD_o       => TxD,
      CTS_i       => '1',
      Ps2Clk_io   => Ps2Clk,
      Ps2Dat_io   => Ps2Dat,
      Ps2MouseClk_io => Ps2MouseClk,
      Ps2MouseDat_io => Ps2MouseDat,
      nkc_DB      => nkc_DB_buf,      
      nkc_ADDR_i  => nkc_ADDR(18 downto 0),  
      nkc_ADDR_i_020_A0 => '0',
      nkc_ADDR_i_020_A1 => '0',
      nkc_nRD_i   => nkc_nRD,   
      nkc_nWR_i   => nkc_nWR,   
      nkc_nIORQ_i => nkc_nIORQ, 
      nkc_nMEMRQ_i=> nkc_nMEMRQ,
      nkc_nWAIT_o => nkc_nWAIT,
      driver_nEN_o=> driver_nEN,
      driver_DIR_o=> driver_DIR,
      UDS_SIZ0_i  => not m68k_nUDS,
      LDS_SIZ1_i  => not m68k_nLDS,
      SD_SCK_o    => open,
      SD_nCS_o    => open,
      SD_MOSI_o   => SD_MOSI,
      SD_MISO_i   => SD_MOSI,
--      VDIP_SCK_o  => VDIP_SCK, 
--      VDIP_CS_o   => VDIP_CS,  
--      VDIP_MOSI_o => VDIP_MOSI,
--      VDIP_MISO_i => '1', --VDIP_MISO,
      IDE_D	     => IDE_D,
      IDE_RD	 => open,
      IDE_WR	 => open,
      IDE_INT	 => '0',
      IDE_IA	 => open,
      IDE_CS0	 => open,
      IDE_CS1	 => open,


      SRAM_ADDR  => SRAM_addr,
      SRAM_DB    => SRAM_DB,
      SRAM_nCS0  => SRAM_nCS0,
      SRAM_nCS1  => SRAM_nCS1,
      SRAM_nWR   => SRAM_nWR,
      SRAM_nOE   => SRAM_nOE,
      SRAM_nBHE  => SRAM_nBHE,
      SRAM_nBLE  => SRAM_nBLE
      );

  RX : entity work.RS_232_RX
    port map(RX => TxD);


  -- clock generation
  Clk     <= not Clk after 12.5 ns;  -- 40 MHz
  CPU_Clk <= not CPU_Clk after 40 ns; -- ~12.5 MHz

  nkc_DB_buf <= nkc_DB after 5 ns when driver_nEN = '0' and driver_DIR = '0' else
                (others => 'Z') after 5 ns;

  nkc_DB     <= nkc_DB_buf(15 downto 0) after 5 ns when driver_nEN = '0' and driver_DIR = '1' else
                (others => 'Z') after 5 ns;

--  SRAM_DB <= std_logic_vector(SRAM_data_o) after 1 ns when (SRAM_ena_o and SRAM_we_o)='1' else
--             (others => 'Z') after 1 ns;

--  SRAM_data_i <= std_ulogic_vector(SRAM_DB);
--  SRAM_nCE    <= not (SRAM_ena_o and not Clk);
--  SRAM_nWR    <= not (SRAM_we_o and not Clk);
--  SRAM_nOE    <= (SRAM_we_o and not Clk);
--  SRAM_Addr   <= std_logic_vector(SRAM_addr_o(16 downto 0)) after 1 ns;    
    
 --   SRAM1_nWR  <= SRAM_nWR ;  after 1 ns; 
 --   SRAM1_nOE  <= SRAM_nOE ;  after 1 ns; 
 --   SRAM1_nBHE <= SRAM_nBHE;  after 1 ns;
 --   SRAM1_nBLE <= SRAM_nBLE;  after 1 ns;
 --   SRAM1_addr <= SRAM_addr;  after 1 ns;

    
  VSRAM0 : entity work.async_512kx16
    generic map (
      ADDR_BITS    => 18,
      DATA_BITS    => 16,
      depth        => 262144,
      TimingInfo   => TRUE,
      TimingChecks => '1'
    )
    port map (
      dump   => do_dump,
      CE_b   => SRAM_nCS0,
      WE_b   => SRAM_nWR,   
      OE_b   => SRAM_nOE,   
      BHE_b  => SRAM_nBHE,
      BLE_b  => SRAM_nBLE,
      A      => SRAM_addr,   
      DQ     => SRAM_DB  
    );
--  VSRAM0 : entity work.SRAM
--    generic map(
--      dump_offset => 0,
--      size        => 2**17,
--      adr_width   => 17
--    )
--    port map(
--      dump => do_dump,
--      nCE => SRAM_nCE,
--      nWE => SRAM_nWR,
--      nOE => SRAM_nOE,
--      A   => SRAM_Addr(16 downto 0),
--      D   => SRAM_DB(7 downto 0)
--    );
--    
--    VSRAM1 : entity work.SRAM
--    generic map(
--      dump_offset => 2,
--      size        => 2**17,
--      adr_width   => 17
--    )
--    port map(
--      dump => do_dump,
--      nCE => SRAM_nCE1,
--      nWE => SRAM_nWR,
--      nOE => SRAM_nOE,
--      A   => SRAM_Addr(16 downto 0),
--      D   => SRAM_DB(7 downto 0)
--    );

  nkc_nIORQ  <= m68k_nAS    after 5 ns when nkc_ADDR(19)='0' else '1';
  nkc_nMEMRQ <= m68k_nAS    after 5 ns when nkc_ADDR(19)='1' else '1';
  nkc_nWR   <= m68k_nRW     after 5 ns when (m68k_nLDS='0' or m68k_nUDS='0') else '1';
  nkc_nRD   <= not m68k_nRW after 5 ns when (m68k_nLDS='0' or m68k_nUDS='0') else '1';


--  vdip_slave: process
--    variable sel_v : boolean;
--    variable rw_v,adr_v,ack_v : std_ulogic;
--    variable data_v : std_ulogic_vector(7 downto 0);
--  begin
--    VDIP_MISO <= '1';
--    ack_v := '0';
--    while true loop
--      sel_v :=false;
--      loop
--        wait until rising_edge(VDIP_SCK);
--        sel_v:= VDIP_CS='1';
--        exit when sel_v;
--       end loop;
--      -- start
--      assert false report "Start of Frame" severity note;
--      assert VDIP_MOSI='1' report "Wrong startbit" severity error;
--      
--      
--      wait until rising_edge(VDIP_SCK);
--      -- rw
--      rw_v := VDIP_MOSI;
--      wait until rising_edge(VDIP_SCK);
--      -- address
--      adr_v := VDIP_MOSI;
--      
--      if rw_v='0' then
--        -- write
--        for i in 7 downto 0 loop
--          wait until rising_edge(VDIP_SCK);
--          data_v(i) := VDIP_MOSI;
--        end loop;
--      else
--        -- read
--        for i in 7 downto 0 loop
--          wait until falling_edge(VDIP_SCK);
--          VDIP_MISO <= data_v(i) after 2 ns;
--        end loop;
--      end if;
--      wait until falling_edge(VDIP_SCK);
--      -- status
--      VDIP_MISO <= ack_v after 2 ns;
--      ack_v := not ack_v;
--      wait until rising_edge(VDIP_SCK);
--      vdip_data <=data_v;  
--    end loop;
--  end process vdip_slave;
  


  -- waveform generation
  WaveGen_Proc: process
--    procedure write_bus(addr : in bit_vector(7 downto 0); data : in bit_vector(7 downto 0)) is
--    begin
--      nkc_ADDR    <= to_stdulogicvector(addr) after 80 ns;
--      wait until CPU_Clk'event and CPU_Clk='0';
--      nkc_DB      <= to_stdlogicvector(data) after 115 ns;
--      wait until CPU_Clk'event and CPU_Clk='1';
--      
--      nkc_nIORQ   <= '0' after 55 ns;
--      nkc_nWR     <= '0' after 60 ns;
--      
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='0';
--      nkc_nIORQ   <= '1' after 60 ns;
--      nkc_nWR     <= '1' after 60 ns;
--      nkc_DB      <= (others => 'Z') after 75 ns;
--      wait until CPU_Clk'event and CPU_Clk='1';
--      
--    end write_bus;
    -- 
    
    procedure write_bus(addr : in bit_vector(19 downto 0); data : in bit_vector(15 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin
      ds_tmp := to_stdlogicvector(ds);
      -- S0
      nkc_ADDR    <= (others => '-');
      nkc_DB      <= (others => 'Z') after 60 ns;
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S1
      nkc_ADDR                <= (others => '0');   
      nkc_ADDR(19 downto 0)   <= to_stdulogicvector(addr(19 downto 0)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S2
      m68k_nAS    <= '0' after 55 ns;
      m68k_nRW    <= '0' after 60 ns;
      nkc_DB      <= to_stdlogicvector(data) after 70 ns;
      
      -- S3 --- 
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S4
      m68k_nUDS    <= not (ds_tmp(1)) after 55 ns;
      m68k_nLDS    <= not (ds_tmp(0)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S6 -- WAIT
      --wait until nkc_nWAIT = 'Z';
      while nkc_nWait = '0' loop wait for 1ns; end loop;
      
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S7
      m68k_nAS    <= '1' after 50 ns;
      m68k_nUDS   <= '1' after 50 ns;
      m68k_nLDS   <= '1' after 50 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S0
    end write_bus;
    
--    procedure write_bus(addr : in bit_vector(7 downto 0); data : in natural) is
--    begin
--      nkc_ADDR    <= to_stdulogicvector(addr) after 80 ns;
--      wait until CPU_Clk'event and CPU_Clk='0';
--      nkc_DB      <= std_logic_vector(to_unsigned(data,8)) after 115 ns;
--      wait until CPU_Clk'event and CPU_Clk='1';
--      
--      
--      nkc_nIORQ   <= '0' after 55 ns;
--      nkc_nWR     <= '0' after 60 ns;
--      
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='0';
--      nkc_nIORQ   <= '1' after 60 ns;
--      nkc_nWR     <= '1' after 60 ns;
--      nkc_DB      <= (others => 'Z') after 75 ns;
--      wait until CPU_Clk'event and CPU_Clk='1';
--      
--    end write_bus;

    procedure write_bus(addr : in natural; data : in natural; ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin
      ds_tmp := to_stdlogicvector(ds);
      -- S0
      nkc_ADDR    <= (others => '-');
      nkc_DB      <= (others => 'Z') after 60 ns;
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S1
      nkc_ADDR                <= (others => '0');
      nkc_ADDR(19 downto 0)   <= std_logic_vector(to_unsigned(data,20)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S2
      m68k_nAS    <= '0' after 55 ns;
      m68k_nRW    <= '0' after 60 ns;
      nkc_DB      <= std_logic_vector(to_unsigned(data,16)) after 70 ns;
      
      -- S3 
      
 
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S4
      m68k_nUDS    <= not (ds_tmp(1)) after 55 ns;
      m68k_nLDS    <= not (ds_tmp(0)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S6 -- WAIT
      --wait until nkc_nWAIT = 'Z';
      while nkc_nWait = '0' loop wait for 1ns; end loop;
      
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S7
      m68k_nAS    <= '1' after 50 ns;
      m68k_nUDS   <= '1' after 50 ns;
      m68k_nLDS   <= '1' after 50 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S0
    end write_bus;
    
    procedure write_bus(addr : in bit_vector(19 downto 0); data : in natural; ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin
      ds_tmp := to_stdlogicvector(ds);
      -- S0
      nkc_ADDR    <= (others => '-');
      nkc_DB      <= (others => 'Z') after 60 ns;
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S1
      nkc_ADDR                <= (others => '0');
      nkc_ADDR(19 downto 0)   <= to_stdulogicvector(addr(19 downto 0)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S2
      m68k_nAS    <= '0' after 55 ns;
      m68k_nRW    <= '0' after 60 ns;
      nkc_DB      <= std_logic_vector(to_unsigned(data,16)) after 70 ns;
      
      -- S3     
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S4
      m68k_nUDS    <= not (ds_tmp(1)) after 55 ns;
      m68k_nLDS    <= not (ds_tmp(0)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S6  --- Wait States ---
      --wait until nkc_nWAIT = 'Z';
      while nkc_nWait = '0' loop wait for 1ns; end loop;
        
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S7
      m68k_nAS    <= '1' after 50 ns;
      m68k_nUDS   <= '1' after 50 ns;
      m68k_nLDS   <= '1' after 50 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S0
    end write_bus;

--    procedure read_bus(addr : in bit_vector(7 downto 0); data : out std_ulogic_vector(7 downto 0)) is
--    begin
--      nkc_ADDR    <= to_stdulogicvector(addr) after 80 ns;
--
--      wait until CPU_Clk'event and CPU_Clk='1';
--      
--      nkc_nIORQ   <= '0' after 55 ns;
--      nkc_nRD     <= '0' after 60 ns;
--      
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='1';
--      wait until CPU_Clk'event and CPU_Clk='0';
--      nkc_nIORQ   <= '1' after 60 ns;
--      nkc_nRD     <= '1' after 60 ns;
--      assert nkc_DB'stable(30 ns) report "Data Read Error" severity error;
--      data        := std_ulogic_vector(nkc_DB);
--      wait until CPU_Clk'event and CPU_Clk='1';
--     end read_bus;

    procedure read_bus(addr : in bit_vector(19 downto 0); data : out std_ulogic_vector(15 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin
      ds_tmp := to_stdlogicvector(ds);
      -- S0
      nkc_ADDR    <= (others => '-');
      nkc_DB      <= (others => 'Z') after 60 ns;
      m68k_nRW    <= '1' after 60 ns;
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S1
      nkc_ADDR                <= (others => '0');
      nkc_ADDR(19 downto 0)   <= to_stdulogicvector(addr(19 downto 0)) after 55 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S2
      m68k_nAS    <= '0' after 55 ns;
      m68k_nUDS   <= not (ds_tmp(1)) after 55 ns;
      m68k_nLDS   <= not (ds_tmp(0)) after 55 ns;
      
      -- S3 --- Wait States ---
      --wait until nkc_nWAIT = 'Z';
      while nkc_nWait = '0' loop wait for 1ns end loop;
      
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S4
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S6
      wait until CPU_Clk'event and CPU_Clk='0';
      -- S7
      assert nkc_DB'stable(10 ns) report "read data error. Data not stable" severity error;
      data := (others => '0');
      for i in 0 to 1 loop
        data(i*8+7 downto i*8)  := std_ulogic_vector(nkc_DB(i*8+7 downto i*8));
      end loop;
      m68k_nAS    <= '1' after 50 ns;
      m68k_nUDS   <= '1' after 50 ns;
      m68k_nLDS   <= '1' after 50 ns;
      
      wait until CPU_Clk'event and CPU_Clk='1';
      -- S0
    end read_bus;

    procedure wait_ready is
      variable tmp_v : std_ulogic_vector(15 downto 0);
    begin
      read_bus(X"00070",tmp_v);
      while tmp_v(2) = '0' loop
        read_bus(X"00070",tmp_v);
      end loop;
    end wait_ready;

    procedure line(x1,y1,x2,y2 : integer) is
      variable dx,dy : integer;
      variable xp,yp,sx,sy : integer;
    begin
      write_bus(X"00078",x1/256);        -- x msb
      write_bus(X"00079",x1 mod 256);   -- x lsb
      write_bus(X"0007A",y1/256);        -- y msb
      write_bus(X"0007B",y1 mod 256);   -- y lsb
      xp:=x1;
      yp:=y1;
      sx := 0;sy :=0;
      if x1>x2 then sx:=1; end if;
      if y1>y2 then sy:=1; end if;
      loop
        dx := abs(x2-xp);
        dy := abs(y2-yp);
--        if dx > 255 or dy > 255 then
--          dx := dx / 2;
--          dy := dy / 2;
--        end if;
        write_bus(X"00074",dx/256);
        write_bus(X"00075",dx mod 256);        -- dx
        write_bus(X"00076",dy/256);
        write_bus(X"00077",dy mod 256);        -- dy
        write_bus(X"00070",17+sx*2+sy*4);
        wait_ready;
        if sx>0 then
          xp := xp - dx;
        else
          xp := xp + dx;
        end if;
        if sy>0 then
          yp := yp - dy;
        else
          yp := yp + dy;
        end if;
        
        exit when xp=x2 and yp=y2;
      end loop;
    end line;
    
    procedure wait_tx_empty is
      variable tmp_v : std_ulogic_vector(15 downto 0);
    begin
      read_bus(X"000F1",tmp_v); -- status register
      while tmp_v(4) = '0' loop
        read_bus(X"000F1",tmp_v);
      end loop;
    end wait_tx_empty;
    
    procedure wait_spi_done is
      variable tmp_v : std_ulogic_vector(15 downto 0);
    begin
      read_bus(X"00000",tmp_v); -- status register
      while tmp_v(0) = '0' loop
        read_bus(X"00000",tmp_v);
      end loop;
    end wait_spi_done;
    
--    procedure wait_vdip_done is
--      variable tmp_v : std_ulogic_vector(15 downto 0);
--    begin
--      read_bus(X"00020",tmp_v); -- status register
--      while tmp_v(7) = '0' loop
--        read_bus(X"00020",tmp_v);
--      end loop;
--    end wait_vdip_done;
    
    procedure send_uart(data : in bit_vector(15 downto 0)) is
    begin
      wait_tx_empty;
      write_bus(X"000F0",data);
    end send_uart;
    
    procedure wait_T1OV is
      variable tmp_v : std_ulogic_vector(15 downto 0);
    begin
      read_bus(X"000F4",tmp_v); -- status register
      while tmp_v(0) = '0' loop
        read_bus(X"000F4",tmp_v);
      end loop;
    end wait_T1OV;
    
    variable read_data : std_ulogic_vector(15 downto 0);
  begin
    -- insert signal assignments here
    do_dump    <= false;
    nkc_DB     <= (others => 'Z');
    nkc_ADDR   <= (others => '0');
--    nkc_nRD    <=  '1';
--    nkc_nWR    <=  '1';
--    nkc_nIORQ  <=  '1';
    m68k_nAS <= '1';
    m68k_nRW <= '1';
    m68k_nLDS <= '1';
    m68k_nUDS <= '1';
    
    reset_n_i  <= '0', '1' after 50 ns;
    wait for 100 ns;
    wait until CPU_Clk'event and CPU_Clk='1';

    write_bus(X"80000",X"1234", "11");  -- SRAM Write
    write_bus(X"80001",X"5678", "11");  -- SRAM Write
    write_bus(X"80002",X"9ABC", "11");  -- SRAM Write
    write_bus(X"80002",X"DEF0", "11");  -- SRAM Write
    
   


    -- Test access to SRAM during Video
    
    report " ----- Test access to SRAM during Video ------ " severity note;
    
    for i in 0 to 1000 loop
        write_bus(524288+i,i, "11");  -- SRAM Write
    end loop;

    do_dump <= true;
    wait for 1 us;
    assert false report "End of simulation" severity failure;
    wait;
  end process WaveGen_Proc;

  Ps2Clk <= 'H';
  Ps2Dat <= 'H';
  Ps2MouseClk <= 'H';
  Ps2MouseDat <= 'H';
  
  

end beh;

