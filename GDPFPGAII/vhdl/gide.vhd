----------------------------------------------------------------------------------
-- Author:         Jens Mewes, Torsten Hemmecke


-- Create Date:    2008
-- Last Change:    2013-11-05
-- Module Name:    GIDE - Behavioral 
-- Project Name:   NKC IDE Interface
-- Target Devices: LFXP6C
-- Tool versions:  
-- Description:    IDE Interface based on the idea of Tillmann Reh's GIDE
--                 The original design was implemented using PAL's and latches
-- Revision        1.0
--						 0.2 - 2008-08-23 - lauffähig
--                       1.0 - code changed for use in wishbone, optimized wait signal generation
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


entity gide is   
				 
    port ( 	--------------------------
	   		-- Data Bus (WishBone)
	   		--------------------------           	
			gide_reset_n					: in  std_logic;
			gide_clk						: in  std_logic;
		
			gide_wbs_address                : in  std_logic_vector(7 downto 0);
			gide_wbs_readdata               : out std_logic_vector(7 downto 0);
			gide_wbs_writedata              : in  std_logic_vector(7 downto 0);
			gide_wbs_ack                    : out std_logic;
			gide_wbs_strobe                 : in  std_logic;
			gide_wbs_cycle                  : in  std_logic; -- ACHTUNG: wird z.Z. nicht beachtet, zeigt aber die Gültigkeit aller anderen Signale an !!
			gide_wbs_write                  : in  std_logic;
			
    		--------------------------
       		-- IDE Signals
       		--------------------------
    		IDE_D							: inout	std_logic_vector (15 downto 0);		-- 16 Bit data bus to/from ide
    		IDE_RD							: out		std_logic;							-- /IORD to ide
            IDE_WR							: out		std_logic;							-- /IOWR to ide
            IDE_INT							: in		std_logic;							-- INT from IDE
           	IDE_CS0							: out		std_logic;								-- CS0 to ide
           	IDE_CS1							: out		std_logic;								-- CS1 to ide           	
           	IDE_IA							: out		std_logic_vector (2 downto 0)		-- A0,A1,A2 to ide
    		
           );
end gide;



architecture Behavioral OF gide IS




signal FSM_CLK			:	std_logic;
signal DATA_D_IDE		:	std_logic_vector(15 downto 0);
signal DATA_IDE_D		:	std_logic_vector(15 downto 0);
signal ADR_BUFFER		:	std_logic_vector(2 downto 0);
signal CS_BUFFER		:	std_logic_vector(1 downto 0);
signal TOGGLE 			: 	std_logic;

signal IDE_WR_EN, D_WR_EN, D_WR_LOW_HIGH 	: std_logic;
signal IDE_WR_SIG, IDE_RD_SIG 				: std_logic;

signal q : std_logic_vector(4 downto 0):= (others => '0'); -- reset condition for simulation

type state_type is (INIT,IDLE,AKTIV,
	RD16_1_1,RD16_1_2,RD16_1_3,RD16_1_4,RD16_1_5,RD16_1_6,RD16_1_7,RD16_1_8,RD16_1_9,RD16_2,
	IDE_RD_1,IDE_RD_2,IDE_RD_3,IDE_RD_4,IDE_RD_5,IDE_RD_6,IDE_RD_7,IDE_RD_8,
	RD8_2,
	WR16_1,WR16_1_1,WR16_2,WR16_2_1,
	WR8,WR8_1,
	IDE_WR_1,IDE_WR_2,IDE_WR_3,IDE_WR_4,IDE_WR_5,IDE_WR_6,IDE_WR_7,
	EOT);  

signal state, next_state: state_type ;  
--signal ws_cnt : std_logic_vector(20 downto 0);

begin

	--FSM_CLK <= gide_clk; -- use original 40MHz clock
	FSM_CLK <= q(1); --
	
	half_clock: process(gide_clk)
	begin    
    if (gide_clk'event and gide_clk = '1') then      
	   q <= q + 1;
	  end if;	  
   end process;
	
	IDE_IA <= ADR_BUFFER(2 downto 0);
	IDE_CS0 <= CS_BUFFER(0);
	IDE_CS1 <= CS_BUFFER(1);
	
	IDE_D <= DATA_D_IDE when IDE_WR_EN = '1' else (others => 'Z');
	IDE_WR <= '0' when IDE_WR_SIG = '1' else '1';
	IDE_RD <= '0' when IDE_RD_SIG = '1' else '1';
	
	
	
	
	output: process (D_WR_EN, D_WR_LOW_HIGH, DATA_IDE_D)
	begin
		if D_WR_EN = '1' then
			if	D_WR_LOW_HIGH = '1' then
				gide_wbs_readdata <= DATA_IDE_D(15 downto 8);
			else
				gide_wbs_readdata <= DATA_IDE_D(7 downto 0);
			end if;
		else	
			gide_wbs_readdata <= (others => 'Z');
		end if;
	end process output;


	-- state clock and synchronize process
	STATE_CLK: process (FSM_CLK, gide_reset_n)
	begin  
		if (gide_reset_n = '0') then  
			state <= INIT;  
			
			IDE_RD_SIG <= '0';
			IDE_WR_SIG <= '0';
			IDE_WR_EN <= '0';
			D_WR_EN <= '0';
			D_WR_LOW_HIGH <= '0';
			
			TOGGLE <= '0';
			ADR_BUFFER <= (others => '0');
			CS_BUFFER <= (others => '1');
			DATA_D_IDE <= (others => '0');
			DATA_IDE_D <= (others => '0');
			
		elsif rising_edge(FSM_CLK) then  
			state <= next_state;
					
			case state is  
				when INIT =>
					TOGGLE <= '0';
					
					ADR_BUFFER <= (others => '0');
					CS_BUFFER <= (others => '1');
					IDE_RD_SIG <= '0';
					IDE_WR_SIG <= '0';
					DATA_D_IDE <= (others => '0');
					DATA_IDE_D <= (others => '0');
					IDE_WR_EN <= '0';
					D_WR_EN <= '0';
					D_WR_LOW_HIGH <= '0';
					
					gide_wbs_ack <= '0';
					
			-------- IDE IDLE ---------------------------------------------------------------------------------
				when IDLE  =>			-- IDE not selected
					IDE_RD_SIG <= '0';
					IDE_WR_SIG <= '0';
					
					IDE_WR_EN <= '0';
					D_WR_EN <= '0';
					D_WR_LOW_HIGH <= '0';
					
					gide_wbs_ack <= '0';
					
			--------- IDE ACCESS -------------------------------------------------------------------------------	
				when AKTIV =>   	-- we have an IDE access
					ADR_BUFFER <= gide_wbs_address(2 downto 0);
					CS_BUFFER <= gide_wbs_address(3) & not gide_wbs_address(3);
					
					gide_wbs_ack <= '0';
					
			------- 16 bit read access ----------------------------------------------------------------------------
				when RD16_1_1 =>				-- IDE setup time max 70ns in mode 0
					
					TOGGLE <= '1';
					
				when RD16_1_2 => null;
					
				when RD16_1_3 => 																	--  8 bit read access, read LSB  
					IDE_RD_SIG <= '1';
					DATA_IDE_D <= IDE_D;	
					
				when RD16_1_4 =>				-- IDED mode0 spec RD/WR low time 165ns
					DATA_IDE_D <= IDE_D;	
					
				when RD16_1_5 =>
					DATA_IDE_D <= IDE_D;						
					
				when RD16_1_9 =>
					IDE_RD_SIG <= '0';
					
					D_WR_EN <= '1';
					D_WR_LOW_HIGH <= '0';
					
					gide_wbs_ack <= '1'; -- send wishbone ack
					
				when RD16_2 =>
					IDE_RD_SIG <= '0';
					
					D_WR_EN <= '1';
					D_WR_LOW_HIGH <= '1';	
					
					gide_wbs_ack <= '1'; -- send wishbone ack
					
			 ------- 8 bit read access ---------------------------------------------------------------------------	
				when IDE_RD_1 =>				-- IDE setup time max 70ns in mode 0
					
					
				when IDE_RD_2 => null;
					
				when IDE_RD_3 => 																	--  8 bit read access, read LSB  
					IDE_RD_SIG <= '1';
					DATA_IDE_D <= IDE_D;	
					
				when IDE_RD_4 =>				-- IDED mode0 spec RD/WR low time 165ns
					DATA_IDE_D <= IDE_D;	
					
				when IDE_RD_5 =>
					DATA_IDE_D <= IDE_D;						

				when RD8_2 =>
					IDE_RD_SIG <= '0';					
					D_WR_EN <= '1';
					D_WR_LOW_HIGH <= '0';
					
					gide_wbs_ack <= '1'; -- send wishbone ack
					
			 ------- 16 bit write access --------------------------------------------------------------------------
				when WR16_1 =>																	-- 16 bit write access
					DATA_D_IDE(7 downto 0) <= gide_wbs_writedata;
					TOGGLE <= '1';							
					
				when WR16_1_1 => --null;																	-- 16 bit write access
					gide_wbs_ack <= '1'; -- send wishbone ack
		
				when WR16_2 =>
					DATA_D_IDE(15 downto 8) <= gide_wbs_writedata;	
					TOGGLE <= '0';
					
				when WR16_2_1 =>
               		
			 -------  8 bit write access ---------------------------------------------------------------------------
				when WR8 =>						-- IDE setup timemax 70ns in mode 0
					DATA_D_IDE(7 downto 0) <= gide_wbs_writedata;					
					
				when WR8_1 => 
				  
			 ------- 8/16 Bit write access common part
				when IDE_WR_1 =>				-- IDED mode0 spec RD/WR low time 165ns
					IDE_WR_EN <= '1';
					IDE_WR_SIG <= '1';
					
				when IDE_WR_2 => null;
					
				when IDE_WR_3 => null;					
					
				when IDE_WR_7 =>
					IDE_WR_SIG <= '0';
					
			 ------- End Of Transmission -----------------------------------------------------------------------------
				when EOT =>										-- End Of Transmission
					ADR_BUFFER <= (others => '0');
					CS_BUFFER <= (others => '1');
					IDE_RD_SIG <= '0';
					IDE_WR_SIG <= '0';
					
					DATA_D_IDE <= (others => '0');	
					DATA_IDE_D <= (others => '0');	
					IDE_WR_EN <= '0';
					D_WR_EN <= '0';
					D_WR_LOW_HIGH <= '0';
					TOGGLE <= '0';
					
					gide_wbs_ack <= '1'; -- send wishbone ack
					
			 ------- end of cycle ----------------------------------------------------------------------------------
				when others =>
					ADR_BUFFER <= (others => '0');
					CS_BUFFER <= (others => '0');
					IDE_RD_SIG <= '0';
					IDE_WR_SIG <= '0';
					
					IDE_WR_EN <= '0';
					D_WR_EN <= '0';
					D_WR_LOW_HIGH <= '0';	
					
					TOGGLE <= '0';
					gide_wbs_ack <= '1'; -- send wishbone ack
					
					
			end case;  
		end if;  
	end process STATE_CLK; 
   
  
	-- define state transitions
	STATE_TRANSITION : process (gide_reset_n, STATE, gide_wbs_strobe,gide_wbs_write, gide_wbs_address, TOGGLE )  
	begin 
		if(gide_reset_n = '0') then
			next_state <= INIT;
		else
			case state is  
				when INIT =>
					next_state <= IDLE;
					
			-------- IDE IDLE ---------------------------------------------------------------------------------
				when IDLE  =>			-- IDE not selected
					if gide_wbs_strobe = '1' then 
						next_state <= AKTIV;					
					else 
						next_state <= IDLE;
					end if;	
					
			--------- IDE ACCESS -------------------------------------------------------------------------------	
				when AKTIV =>   	-- we have an IDE access
					if gide_wbs_strobe /='1' then  -- back to IDLE
						next_state <= IDLE;
					elsif(gide_wbs_address = X"18" and gide_wbs_write = '0' and TOGGLE = '0') then  -- 16 bit read access low
						next_state <= RD16_1_1;
					elsif(gide_wbs_address = X"18" and gide_wbs_write = '0' and TOGGLE = '1') then  -- 16 bit read access
						next_state <= RD16_2;
					elsif(gide_wbs_address /= X"18" and gide_wbs_write = '0') then --  8 bit read access
					 next_state <= IDE_RD_1;
					elsif(gide_wbs_address = X"18" and gide_wbs_write = '1' and TOGGLE = '0') then  -- 16 bit write access low
					 next_state <= WR16_1;
					elsif(gide_wbs_address = X"18" and gide_wbs_write = '1' and TOGGLE = '1') then  -- 16 bit write access high
					 next_state <= WR16_2;
					elsif(gide_wbs_address /= X"18" and gide_wbs_write = '1') then --  8 bit write access 
					 next_state <= WR8;
					else
					  next_state <= AKTIV;
					end if;

			------- 16 bit read access ----------------------------------------------------------------------------
				when RD16_1_1 =>				-- IDE setup time max 70ns in mode 0
					next_state <= RD16_1_2;
					
				when RD16_1_2 =>
					next_state <= RD16_1_3;
					
				when RD16_1_3 => 																	--  8 bit read access, read LSB  
					next_state <= RD16_1_4;
					
				when RD16_1_4 =>				-- IDED mode0 spec RD/WR low time 165ns
					next_state <= RD16_1_5;
					
				when RD16_1_5 =>
					next_state <= RD16_1_9;
					
				when RD16_1_9 =>
					if gide_wbs_strobe = '1' and gide_wbs_write = '0' then
						next_state <= RD16_1_9;          	
					else
						next_state <= IDLE;
					end if;

				when RD16_2 =>
					if gide_wbs_strobe = '1' and gide_wbs_write = '0' then  
						next_state <= RD16_2;     	
					else
						next_state <= EOT;	-- ready
					end if;
	
			 ------- 8 bit read access ---------------------------------------------------------------------------	
				when IDE_RD_1 =>				-- IDE setup time max 70ns in mode 0
					next_state <= IDE_RD_2;
					
				when IDE_RD_2 =>
					next_state <= IDE_RD_3;
					
				when IDE_RD_3 => 																	--  8 bit read access, read LSB  
					next_state <= IDE_RD_4;
					
				when IDE_RD_4 =>				-- IDED mode0 spec RD/WR low time 165ns
					next_state <= IDE_RD_5;
					
				when IDE_RD_5 =>
					next_state <= RD8_2;				

				when RD8_2 =>
					if gide_wbs_strobe = '1' and gide_wbs_write = '0' then  
						next_state <= RD8_2;          	
					else
						next_state <= EOT;	-- ready
					end if;

			 ------- 16 bit write access --------------------------------------------------------------------------
				when WR16_1 =>																	-- 16 bit write access
					next_state <= WR16_1_1;
	

				when WR16_1_1 =>
					if gide_wbs_strobe = '1' then  -- ready
						next_state <= WR16_1_1;
					else
						next_state <= IDLE;
					end if;					
			
				when WR16_2 =>					
					next_state <= IDE_WR_1;
		
			 -------  8 bit write access ---------------------------------------------------------------------------
				when WR8 =>				-- IDE setup timemax 70ns in mode 0			
					next_state <= IDE_WR_1;
			
			 -------- 8/16-Bit write access common part -------
				when IDE_WR_1 =>
					next_state <= IDE_WR_2;
					
				when IDE_WR_2 =>
					next_state <= IDE_WR_3;
					
				when IDE_WR_3 =>
					next_state <= IDE_WR_7;
	
				when IDE_WR_7 =>
					next_state <= EOT;
				
			 ------- End Of Transmission -----------------------------------------------------------------------------
				when EOT =>										-- End Of Transmission
					if (gide_wbs_strobe = '1') then  
						next_state <= EOT;
					else
						next_state <= IDLE;	-- back to IDLE
					end if;
			 ------- end of cycle ----------------------------------------------------------------------------------
				when others =>
					next_state <= IDLE;
					
			end case;  
		end if;
	end process STATE_TRANSITION; 

end Behavioral;

