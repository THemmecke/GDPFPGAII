----------------------------------------------------------------------------------
-- Author:         Torsten Hemmecke

-- Create Date:    2012-03-13
-- Last Change:    2012-03-13
-- Module Name:    GDP-FPGA-II-IC1 - Behavioral 
-- Project Name:   NKC GDP-FPGA II CARD
-- Target Devices: Xilinx XC9536-TQFP44-3v3
-- Tool versions:  
-- Description:    GDP-FPGA II  
-- Revision        0.1
--
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity IC1 is   
				 
    port ( 	
				--- ECB BUS SIGNALS (5V)
           	B_A			: in		std_logic_vector (29 downto 20);			-- ECB Address Bus 
           																		-- => entspricht A(23 downto 21) bei 68000 (24 Addressleitungen)
           																		-- => entspricht A(31 downto 22) bei 68020 (32 Addressleitungen)
           																		
           	B_IRQ			: out		std_logic_vector (1 downto 0);			-- ECB IRQ0,IRQ1
				B_INT			: out		std_logic;										-- ECB INT
				B_NMI			: out		std_logic;										-- ECB NMI
				B_WAIT		: out		std_logic;										-- ECB WAIT
				B_IORQ		: in		std_logic;										-- ECB IORQ
				B_MEMRQ		: in		std_logic;										-- ECB MEMRQ
				B_SIZ0		: in		std_logic;										-- ECB SIZ0
				B_SIZ1		: in		std_logic;										-- ECB SIZ1
           	B_RESET		: in		std_logic;										-- ECB RESET
				
				-- LOCAL SIGNALS (3.3V)
				F_IRQ			: in		std_logic_vector (1 downto 0);			-- LOCAL IRQ0,IRQ1
				F_INT			: in		std_logic;										-- LOCAL INT
				F_NMI			: in		std_logic;										-- LOCAL NMI
				F_WAIT		: in		std_logic;										-- LOCAL WAIT
				F_IORQ		: out		std_logic;										-- LOCAL IORQ
				F_MEMRQ		: out		std_logic;										-- LOCAL MEMRQ
				F_SIZ0		: out    std_logic;										-- LOCAL SIZ0
				F_SIZ1		: out		std_logic;										-- LOCAL SIZ1
           	F_RESET		: out		std_logic;										-- LOCAL RESET
				
				-- DEBUG Signals
				
				JP2			: out std_logic;
				JP3			: out std_logic;
				JP4			: out std_logic;
				JP5			: out std_logic
           );
end IC1;

architecture Behavioral OF IC1 IS

constant MEM_BASE_c : std_logic_vector (29 downto 20) := "0000000111"; --  => 0x070.0000 * cpu (68020: range 0x100000 == 1MB, start at  28MB)



signal is_mem_cs_s 		: std_logic;
signal is_io_cs_s 		: std_logic;
signal is_gdp_mem_cs_s 	: std_logic;


begin

	-- pass through signals
	-- ECB==>LOCAL
	F_RESET <= B_RESET;		
	F_IORQ <= B_IORQ;
	F_SIZ0 <= B_SIZ0;
	F_SIZ1 <= B_SIZ1;
	
	
	-- LOCAL==>ECB
	B_INT <= '0' when F_INT = '0' else 'Z';
	
	B_NMI <= 'Z';
	B_IRQ <= (others => 'Z');
			
	-- WAIT SIGNAL
	B_WAIT <= '0' when F_WAIT = '0' else 'Z';
	
	
	-- memory mapped access:
	is_mem_cs_s <= '1' when B_MEMRQ = '0' else '0';
	
	CPU: Process(F_NMI)
	begin
	-- 68020
	if( F_NMI = '0' ) then	-- F_NMI auf LATTICE mit is68000 verbunden !!  i.e. if( is68000 = '0' )
	
	-- is_gdp_mem_cs_s <= is_mem_cs_s when B_A = MEM_BASE_c else '0';
	 if(B_A = MEM_BASE_c) then is_gdp_mem_cs_s <= is_mem_cs_s; else is_gdp_mem_cs_s <= '0'; end if;
	-- 68000 -> weniger Adressleitungen dekodieren oder alternativ auf der 68000CPU-Karte auf Null ziehen (PullDown)
	else
	-- is_gdp_mem_cs_s <= is_mem_cs_s when B_A(22 downto 20) = MEM_BASE_c(22 downto 20) else '0';
	 if(B_A(22 downto 20) = MEM_BASE_c(22 downto 20)) then is_gdp_mem_cs_s <= is_mem_cs_s; else is_gdp_mem_cs_s <= '0'; end if; 
	end if;
	end process;
	
	--is_gdp_mem_cs_s <= is_mem_cs_s when B_A = MEM_BASE_c else '0';
	
	F_MEMRQ <= '0' when is_gdp_mem_cs_s = '1' else '1';
	
	-- debug
	JP5 <= F_IRQ(1);
	JP4 <= '1' when is_gdp_mem_cs_s = '1' else '0'; 
	JP3 <= F_NMI;
	JP2 <= F_IRQ(0);
	--JP2 <= B_IORQ;
	
	

end Behavioral;

