-------------------------------------------------------------------------
-- Company     : 
-- Author(s)   : 
-- 
-- Creation Date : 2013-10-03
-- File          : sram.vhd
--
-- Abstract : 
-- Connect slaves to mwb16 from wrapper
--- Version 1.0: Connection to 68020
----------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

Entity sram is
    port
    (
	
        -- sram_swb16 connection
		sram_reset_n					   : in  std_logic;
		sram_clk						   : in  std_logic;
		
        sram_wbs_address                : in  std_logic_vector(18 downto 0);
        sram_wbs_readdata               : out   std_logic_vector(15 downto 0);
        sram_wbs_writedata              : in  std_logic_vector(15 downto 0);
        sram_wbs_ack                    : out   std_logic;
		sram_wbs_sel		            : in std_logic_vector(1 downto 0);   -- byte lane selct signals (16 bit bus)
        sram_wbs_strobe                 : in  std_logic;
        sram_wbs_cycle                  : in  std_logic;
        sram_wbs_write                  : in  std_logic;

		-- sram external connections
		
		SRAM_ADDR						: out std_logic_vector(17 downto 0);
		SRAM_DB							: inout std_logic_vector(15 downto 0);
		
		SRAM_nWR						: out std_logic;
		SRAM_nOE						: out std_logic;
		SRAM_nCS0						: out std_logic;
		SRAM_nCS1						: out std_logic;
		SRAM_nBHE						: out std_logic;
		SRAM_nBLE						: out std_logic		     
    );
end entity;

architecture sram_1 of sram is

 type state_t is(idle_e,read_e,write_e);
 signal state, next_state : state_t;
	
 signal set_ram_address :  std_logic;
 signal next_ram_cs :  std_logic;
 signal ack,next_ack :  std_logic;
 signal next_wr_data :  std_logic_vector(15 downto 0);
 signal next_rd_data :  std_logic_vector(15 downto 0);
		
		
begin

	 sram_wbs_ack <= sram_wbs_strobe;
	 
  ----------------------------------------------------------------------------
	-- address decoding
	-- ----------------------------------------------------------------------------	 
	
	 SRAM_ADDR(17 downto 0) <= sram_wbs_address(17 downto 0);
	 
	 SRAM_nCS0 <= '0' when (sram_wbs_address(18) = '0' and sram_wbs_strobe = '1') else '1';
	 SRAM_nCS1 <= '0' when (sram_wbs_address(18) = '1' and sram_wbs_strobe = '1') else '1';
	 
	 SRAM_nBLE <= '0' when (sram_wbs_sel(0)='1') else '1';
	 SRAM_nBHE <= '0' when (sram_wbs_sel(1)='1') else '1';	 
	 
	 SRAM_nWR  <= '0' when (sram_wbs_write = '1') else '1';
	 SRAM_nOE  <= '0' when (sram_wbs_write = '0') else '1';
	 	 
	 sram_wbs_readdata <= SRAM_DB;
	 
	 SRAM_DB <= sram_wbs_writedata when (sram_wbs_write = '1') else (others => 'Z');
	 
	 
-- 	 -- define state changes
-- 	 process (sram_clk, sram_reset_n)
-- 	 begin
-- 	 	next_state <= state;
-- 	 	
-- 	 	set_ram_address <= '0';
-- 	 	next_ram_cs <= '0';
-- 	 	next_ack <= '0';
-- 	 		
-- 	 	case state is
-- 	 		when idle_e =>
-- 	 			if(sram_wbs_strobe = '1') then
-- 	 				if(sram_wbs_write = '1') then
-- 	 					next_state <= write_e;
-- 	 					set_ram_address <= '1';
-- 	 					next_ram_cs <= '1';
-- 	 					next_wr_data <= sram_wbs_writedata;
-- 	 				else
-- 	 					next_state <= read_e;
-- 	 					set_ram_address <= '1';
-- 	 					next_ram_cs <= '1';
-- 	 				end if;
-- 	 			end if;
-- 	 				
-- 	 		when read_e =>
-- 	 			next_ack <= '1';
-- 	 			set_ram_address <= '1';
-- 	 			next_ram_cs <= '1';
-- 	 			if(sram_wbs_strobe = '0') then
-- 	 				next_state <= idle_e;
-- 	 				set_ram_address <= '0';
-- 	 				next_ram_cs <= '0';	 				
-- 	 			end if;
-- 	 			
-- 	 		when write_e =>
-- 	 			next_ack <= '1';
-- 	 			set_ram_address <= '1';
-- 	 			next_ram_cs <= '1';
-- 	 			if(sram_wbs_strobe = '0') then
-- 	 				next_state <= idle_e;
-- 	 				set_ram_address <= '0';
-- 	 				next_ram_cs <= '0';
-- 	 			end if;
-- 	 		when others =>
-- 	 			next_state <= idle_e;
-- 	 	end case;
-- 
-- 	 end process;
-- 	 
-- 	 -- set signals
-- 	 process (sram_clk, sram_reset_n)
-- 	 begin
-- 	 	if sram_reset_n = '0' then
-- 	 		SRAM_ADDR(17 downto 0) <= (others => '0');
-- 	 		SRAM_nCS0 <= '0';
-- 	 		SRAM_nCS1 <= '0';
-- 	 		SRAM_nBLE <= '0';
-- 	 		SRAM_nBHE <= '0'; 
-- 	 		SRAM_nWR  <= '0';
-- 	 		SRAM_nOE  <= '0';
-- --	 		SRAM_DB <= (others => 'Z');
-- --	 		sram_wbs_readdata <= (others => '0');
-- 	 		
-- 	 	elsif rising_edge(sram_clk) then
-- 	 		state <= next_state;
-- 	 		ack <= next_ack;
-- 	 		
-- 	 		if set_ram_address  = '1' then
--           	SRAM_ADDR(17 downto 0) <= sram_wbs_address(17 downto 0);          	
-- 	 			SRAM_nBLE <= not sram_wbs_sel(0);
-- 	 			SRAM_nBHE <= not sram_wbs_sel(1);	 
-- 	 			SRAM_nWR  <= not sram_wbs_write;
-- 	 			SRAM_nOE  <= sram_wbs_write;
--         	end if;
--         	
-- 			if next_ram_cs = '1' then 
-- 				SRAM_nCS0 <= sram_wbs_address(18);
-- 				SRAM_nCS1 <= not sram_wbs_address(18);
-- 			end if;
-- 	 	
-- 	 		
-- 	 		
-- 	 	end if;
-- 	 end process;
-- 	 
-- 	 sram_wbs_ack <= ack;
-- 	 
-- --	 SRAM_ADDR(17 downto 0) <= sram_wbs_address(17 downto 0);          	
-- --	 SRAM_nBLE <= not sram_wbs_sel(0);
-- --	 SRAM_nBHE <= not sram_wbs_sel(1);	 
-- --	 SRAM_nWR  <= not sram_wbs_write;
-- --	 SRAM_nOE  <= sram_wbs_write;
	 
	 
     
 end architecture sram_1;