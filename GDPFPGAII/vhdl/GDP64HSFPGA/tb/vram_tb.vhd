--
-- Copyright 1991-2014 Mentor Graphics Corporation
--
-- All Rights Reserved.
--
-- THIS WORK CONTAINS TRADE SECRET AND PROPRIETARY INFORMATION WHICH IS THE PROPERTY OF 
-- MENTOR GRAPHICS CORPORATION OR ITS LICENSORS AND IS SUBJECT TO LICENSE TERMS.
--   
library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.gdp_global.all;
--use Work.Types.all;

entity test_vram is
	generic(INT_CHR_ROM_g : boolean := true); 
--    PORT ( count : BUFFER bit_vector(8 downto 1));
end;

architecture only of test_vram is

component gdp_vram
    generic(INT_CHR_ROM_g : boolean := true);
    port (
      clk_i             : in  std_ulogic;
      clk_en_i          : in  std_ulogic;
      reset_n_i         : in  std_ulogic;
	  blank_i			: in  std_ulogic;
	  -- host port (read & write 16bit)
	  host_req_i        : in std_ulogic;
      host_wr_i         : in std_ulogic;
      host_addr_i       : in std_ulogic_vector(18 downto 0);
      host_data_o    : out std_ulogic_vector(15 downto 0);
      host_data_i    : in std_ulogic_vector(15 downto 0);
      host_sel_i		: in std_ulogic_vector(1 downto 0);
      host_busy_o       : out std_ulogic;
      host_ack_o        : out std_ulogic;
      -- kernel port (read & write)
      kernel_clk_en_i   : in  std_ulogic;
      kernel_req_i      : in  std_ulogic;
      kernel_wr_i       : in  std_ulogic;
      kernel_addr_i     : in  std_ulogic_vector(17 downto 0);
      kernel_data_i     : in  std_ulogic_vector(7 downto 0);
      kernel_data_o     : out std_ulogic_vector(7 downto 0);
      kernel_busy_o     : out std_ulogic;
      kernel_ack_o      : out std_ulogic;
      chr_rom_addr_i    : in  std_ulogic_vector(8 downto 0);
      chr_rom_data_o    : out std_ulogic_vector(7 downto 0);
      chr_rom_ena_i     : in  std_ulogic;
      chr_rom_busy_o    : out std_ulogic;
      -- video port (only read)
      rd_req_i          : in  std_ulogic;
      rd_addr_i         : in  std_ulogic_vector(17 downto 0);
      rd_data_o         : out std_ulogic_vector(7 downto 0);
      rd_busy_o         : out std_ulogic;
      rd_ack_o          : out std_ulogic;
      -- SRAM / ROM control signals
      sram_addr_o       : out std_ulogic_vector(18 downto 0);
      sram_data_o       : out std_ulogic_vector(15 downto 0);
      sram_data_i       : in  std_ulogic_vector(15 downto 0);
      sram_sel_o        : out std_ulogic_vector(1 downto 0);
      sram_we_o         : out std_ulogic;
	  sram_cs_o			: out std_ulogic;
	  sram_ack_i		  : in std_ulogic;
      -- ext. ROM signals
      rom_ena_o         : out std_ulogic;
	  --------------------------
      -- Monitoring (Debug) signals
      --------------------------
      monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
    );
  end component;

SIGNAL clk   : std_ulogic := '0';
SIGNAL nreset : std_ulogic := '1';

SIGNAL host_req   : std_ulogic := '0';
SIGNAL host_wr   : std_ulogic := '0';
SIGNAL host_addr   : std_ulogic_vector(18 downto 0) := (others =>'0');
SIGNAL host_rd_data   : std_ulogic_vector(15 downto 0):= (others =>'0');
SIGNAL host_wr_data   : std_ulogic_vector(15 downto 0):= (others =>'0');
SIGNAL host_sel   : std_ulogic_vector(1 downto 0):= (others =>'0');
SIGNAL host_busy   : std_ulogic := '0';
SIGNAL host_ack   : std_ulogic := '0';
SIGNAL kernel_req   : std_ulogic := '0';
SIGNAL kernel_wr   : std_ulogic := '0';
SIGNAL kernel_addr   : std_ulogic_vector(17 downto 0):= (others =>'0');
SIGNAL kernel_wr_data   : std_ulogic_vector(7 downto 0):= (others =>'0');
SIGNAL kernel_rd_data   : std_ulogic_vector(7 downto 0):= (others =>'0');
SIGNAL kernel_busy   : std_ulogic := '0';
SIGNAL kernel_ack   : std_ulogic := '0';
SIGNAL chr_rom_addr   : std_ulogic_vector(8 downto 0):= (others =>'0');
SIGNAL chr_rom_data   : std_ulogic_vector(7 downto 0):= (others =>'0');
SIGNAL chr_rom_ena   : std_ulogic := '0'; 
SIGNAL chr_rom_busy   : std_ulogic := '0';
SIGNAL vid_rd_req   : std_ulogic := '0';
SIGNAL vid_rd_addr   : std_ulogic_vector(17 downto 0):= (others =>'0');
SIGNAL vid_rd_data   : std_ulogic_vector(7 downto 0):= (others =>'0');
SIGNAL vid_rd_busy   : std_ulogic := '0';
SIGNAL vid_rd_ack   : std_ulogic := '0';
SIGNAL sram_addr_o   : std_ulogic_vector(18 downto 0):= (others =>'0');
SIGNAL sram_data_o   : std_ulogic_vector(15 downto 0):= (others =>'0');
SIGNAL sram_data_i   : std_ulogic_vector(15 downto 0):= (others =>'0');
SIGNAL sram_sel_o   : std_ulogic_vector(1 downto 0):= (others =>'0');
SIGNAL sram_cs_o   : std_ulogic := '0';
SIGNAL sram_we_o   : std_ulogic := '0';
SIGNAL sram_ack_i   : std_ulogic := '0';
SIGNAL monitoring_o : std_ulogic_vector(nr_mon_sigs_c-1 downto 0):= (others =>'0');
SIGNAL blank : std_ulogic := '0';



begin

dut : gdp_vram
    generic map(INT_CHR_ROM_g => INT_CHR_ROM_g)
    port map(
      clk_i           => clk,
      clk_en_i        => '1',
      reset_n_i       => nreset,
	  blank_i		  	=> blank,
	  
	  -- host port (read & write 16bit)
	  host_req_i        => host_req,
      host_wr_i         => host_wr,
      host_addr_i       => host_addr,
      host_data_o    	=> host_rd_data,
      host_data_i    	=> host_wr_data,
      host_sel_i		=> host_sel,
      host_busy_o       => host_busy,
      host_ack_o        => host_ack,
	  
      kernel_clk_en_i 	=> '1',
      kernel_req_i    	=> kernel_req,
      kernel_wr_i     	=> kernel_wr,
      kernel_addr_i   	=> kernel_addr,
      kernel_data_i   	=> kernel_wr_data,
      kernel_data_o   	=> kernel_rd_data,
      kernel_busy_o   	=> kernel_busy,
      kernel_ack_o    	=> kernel_ack,
      chr_rom_addr_i  	=> chr_rom_addr,
      chr_rom_data_o  	=> chr_rom_data,
      chr_rom_ena_i   	=> chr_rom_ena, 
      chr_rom_busy_o  	=> chr_rom_busy,
      rd_req_i        	=> vid_rd_req,
      rd_addr_i       	=> vid_rd_addr,
      rd_data_o       	=> vid_rd_data,
      rd_busy_o       	=> vid_rd_busy,
      rd_ack_o        	=> vid_rd_ack,
      sram_addr_o     	=> sram_addr_o,
      sram_data_o     	=> sram_data_o,
      sram_data_i     	=> sram_data_i,
      sram_sel_o      	=> sram_sel_o,
	  sram_cs_o		 	=> sram_cs_o,
      sram_we_o       	=> sram_we_o,
	  sram_ack_i     	=> sram_ack_i,
      --rom_ena_o       => rom_ena_o
	  -- debug out
	  monitoring_o		=> monitoring_o
    );

	clock : PROCESS
	   begin
	   wait for 10 ns; clk  <= not clk; -- cycle time 20ns ==  50MHz
	end PROCESS clock;
	
	
	stimulus : PROCESS
	   begin
	   wait for 5 ns; nreset  <= '0';
	   wait for 4 ns; nreset  <= '1';
	   wait;
	end PROCESS stimulus;

  	
  	  -- waveform generation ***************************************************************************
  	VRAM_HOST_ACCESS: process
  	 
  	  
  	 procedure host_write(addr : in bit_vector(19 downto 0); data : in bit_vector(15 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
     begin
    	ds_tmp := to_stdlogicvector(ds);
    	
    	host_req <= '1';
		host_wr <= '1';
		host_addr <= to_stdulogicvector(addr(18 downto 0));
		host_wr_data <= to_stdulogicvector(data(15 downto 0));
		host_sel <= ds_tmp;
		
		wait until host_ack = '1';
		wait for 1 ns;
		
		host_req <= '0';
		host_wr <= '0';
		host_addr <= (others => '0');
		host_wr_data <= (others => '0');
		host_sel <= (others => '0');
		
		wait for 50 ns;
    end host_write; 
    
    procedure host_read(addr : in bit_vector(19 downto 0); data : out std_ulogic_vector(15 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin 
    	ds_tmp := to_stdlogicvector(ds);
    	
    	host_req <= '1';
		host_wr <= '0';
		host_addr <= to_stdulogicvector(addr(18 downto 0));		
		host_sel <= ds_tmp;
		
		wait until host_ack = '1';	
		wait for 10 ns;
		
		for i in 0 to 1 loop
        	data(i*8+7 downto i*8)  := std_ulogic_vector(host_rd_data(i*8+7 downto i*8));
      	end loop;
		
		host_req <= '0';
		host_wr <= '0';
		host_addr <= (others => '0');
		host_wr_data <= (others => '0');
		host_sel <= (others => '0');
		wait for 50 ns;
    end host_read;
  	  
    procedure kernel_write(addr : in bit_vector(19 downto 0); data : in bit_vector(15 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
     begin    	
    	
    	kernel_req <= '1';
		kernel_wr <= '1';
		kernel_addr <= to_stdulogicvector(addr(17 downto 0));
		kernel_wr_data <= to_stdulogicvector(data(7 downto 0));
		
		wait until kernel_ack = '1';
		wait for 1 ns;
		
		kernel_req <= '0';
		kernel_wr <= '0';
		kernel_addr <= (others => '0');
		kernel_wr_data <= (others => '0');
		wait for 50 ns;
    end kernel_write; 
    
    procedure kernel_read(addr : in bit_vector(19 downto 0); data : out std_ulogic_vector(7 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin 
    	    
    	kernel_req <= '1';
		kernel_wr <= '0';
		kernel_addr <= to_stdulogicvector(addr(17 downto 0));		
		
		
		wait until kernel_ack = '1';	
		wait for 1 ns;
		
        data  := std_ulogic_vector(kernel_rd_data);

		kernel_req <= '0';
		kernel_wr <= '0';
		kernel_addr <= (others => '0');
		kernel_wr_data <= (others => '0');
		wait for 50 ns;
    end kernel_read;
    
    
    
    variable rd_data16 : std_ulogic_vector(15 downto 0);
  variable rd_data8 : std_ulogic_vector(7 downto 0);	  
  	  
  	  
  	  
  	begin
	  report "Start VRAM 01 Initial Process..."	;
	 
  	  wait for 100 ns; -- Reset abwarten
  	  
  	  --host_write(X"00001", X"1234");
  	  host_read (X"01001", rd_data16);
  	  host_read (X"01002", rd_data16);
  	  host_read (X"01003", rd_data16);
  	  host_read (X"01004", rd_data16);
  	  host_read (X"01005", rd_data16);
  	  host_read (X"01006", rd_data16);
  	  host_read (X"01007", rd_data16);
  	  host_read (X"01008", rd_data16);  	    	 
  	  
  	  wait for 10ns;
  	  
  	  
  	  assert false report "End of simulation" severity failure;
  	  wait;
  	end process VRAM_HOST_ACCESS;
  
  	
  	-- waveform generation ***************************************************************************
  	
	VRAM_VID_ACCESS: process
  	 
  	  
  	
    
    procedure vid_read(addr : in bit_vector(19 downto 0); data : out std_ulogic_vector(7 downto 0); ds : in bit_vector(1 downto 0):="01") is
      variable ds_tmp : std_logic_vector(1 downto 0);
    begin 
    	    
    	vid_rd_req <= '1';
		vid_rd_addr <= to_stdulogicvector(addr(17 downto 0));		

		wait until vid_rd_ack = '1';	
		wait for 1 ns;
		
        data  := std_ulogic_vector(vid_rd_data);

		vid_rd_req <= '0';
		vid_rd_addr <= (others => '0');
		wait until clk'event and clk='1';
    end vid_read;
    
    variable rd_data16 : std_ulogic_vector(15 downto 0);
  variable rd_data8 : std_ulogic_vector(7 downto 0);	  
  	  
  	  
  	  
  	begin
	  report "Start VRAM 01 Initial Process..."	;
	 
  	  wait for 100 ns; -- Reset abwarten
  	    	   	    	  
  	  vid_read (X"00F01", rd_data8);
  	  vid_read (X"00F02", rd_data8);
  	  vid_read (X"00F03", rd_data8);
  	  vid_read (X"00F04", rd_data8);
  	  vid_read (X"00F05", rd_data8);
  	  vid_read (X"00F06", rd_data8);
  	  vid_read (X"00F07", rd_data8);
  	  vid_read (X"00F08", rd_data8);
  	  vid_read (X"00F09", rd_data8);
  	  
  	  wait for 10ns;
  	  
  	  
  	  assert false report "End of simulation" severity failure;
  	  wait;
  	end process VRAM_VID_ACCESS;

end only;

