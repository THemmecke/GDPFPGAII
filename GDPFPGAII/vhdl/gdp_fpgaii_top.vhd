--------------------------------------------------------------------------------
-- Project     : GDPFPGAII
-- Module      : GDP 936X Display processor and Peripherals - Toplevel for Lattice FPGA
-- File        : gdp_fpgaii_top.vhd
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
--use work.DffGlobal.all;
use work.gdp_global.all;
use work.gdp_fpgaii_global.all;


use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity gdp_fpgaii_top is
  generic(sim_g      : boolean := false);
  port(
		reset_n_i     : in  std_logic;
        clk_i         : in  std_logic;
--       addr_sel_i    : in  std_logic;
       --------------------------
       -- NKC Bus
       --------------------------
       nkc_DB        : inout std_logic_vector(15 downto 0);
       nkc_ADDR_i    : in std_logic_vector(18 downto 0);
	   nkc_ADDR_i_020_A0 : in std_logic;
	   nkc_ADDR_i_020_A1 : in std_logic;
       nkc_nRD_i     : in std_logic;
       nkc_nWR_i     : in std_logic;
       nkc_nIORQ_i   : in std_logic;
	   nkc_nMEMRQ_i  : in std_logic;
       driver_nEN_o  : out std_logic;
       driver_DIR_o  : out std_logic;  
	   
	   UDS_SIZ0_i	 : in std_logic;
	   LDS_SIZ1_i	 : in std_logic;
	   
       nIRQ_o        : out std_logic;	   	   
	   nNMI_o		 : out std_logic; 	   
	   IRQ0_o	 	 : out std_logic; 
	   IRQ1_o	 	 : out std_logic; 
	   
	   nkc_nWAIT_o	 : out std_logic; 
       --------------------------
       -- UART Receiver
       --------------------------
       RxD_i    : in  std_logic;
       TxD_o    : out std_logic;
       RTS_o    : out std_logic;
       CTS_i    : in  std_logic;
       --------------------------
       -- PS/2 Keyboard signals
       --------------------------
       -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
       -- PS/2 bus. In all other cases an input would be sufficient.
       Ps2Clk_io    : inout std_logic;
       -- PS/2 data line. Bidirectional for reading and writing data.
       Ps2Dat_io    : inout std_logic;
       --------------------------
       -- PS/2 Mouse signals
       --------------------------
       -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
       -- PS/2 bus. In all other cases an input would be sufficient.
       Ps2MouseClk_io    : inout std_logic;
       -- PS/2 data line. Bidirectional for reading and writing data.
       Ps2MouseDat_io    : inout std_logic;
       --------------------------
       -- Audio DAC-PWM out
       -- This DAC requires an external RC low-pass filter:
       --
       --   pwm_out 0---XXXXX---+---0 analog audio
       --                3k3    |
       --                      === 4n7
       --                       |
       --                      GND
       --------------------------
       PWM_OUT_o   : out std_logic;
       --------------------------
       -- Video out
       --------------------------
       Red_o      : out std_logic_vector(2 downto 0);
       Green_o    : out std_logic_vector(2 downto 0);
       Blue_o     : out std_logic_vector(2 downto 0);
       Hsync_o    : out std_logic;
       Vsync_o    : out std_logic;
       --------------------------
       -- SPI-Signals (SD-Card)						
       --------------------------
       SD_SCK_o  : out std_logic;
       SD_nCS_o  : out std_logic_vector(1 downto 0);
       SD_MOSI_o : out std_logic;
       SD_MISO_i : in  std_logic;       
       --------------------------
       -- Video-Memory data bus				
       --------------------------
       SRAM_nCS0    : out std_logic;
       SRAM_nCS1   : out std_logic;       
	   SRAM_ADDR   : out std_logic_vector(18 downto 0);	
       SRAM_DB     : inout std_logic_vector(15 downto 0);
       SRAM_nWR    : out std_logic;
       SRAM_nOE    : out std_logic;
	   SRAM_nBHE	: out std_logic;						
	   SRAM_nBLE	: out std_logic;						
	   --------------------------
       -- GIDE
       --------------------------
	   	IDE_D		: inout std_logic_vector(15 downto 0);
		IDE_RD		: out std_logic;
		IDE_WR		: out std_logic;
		IDE_INT		: in std_logic;
		IDE_IA		: out std_logic_vector(2 downto 0);
		IDE_CS0		: out std_logic;
		IDE_CS1		: out std_logic    
       --------------------------
       -- Debug Signals - GDP
       --------------------------
--       debug_o      : out std_logic_vector(nr_mon_sigs_c-1 downto 0);
--       sample_clk_o : out std_logic
       );
end gdp_fpgaii_top;


architecture rtl of gdp_fpgaii_top is

-- define components
	
---- gdphs module ---------------------------------
  component gdp_lattice_top is
  port(
  
		--------------------------
	   	-- Data Bus (WishBone)
	   	--------------------------           	
		gdphs_reset_n					: in  std_logic;
		gdphs_clk						: in  std_logic;
		
		gdphs_wbs_address                : in  std_logic_vector(19 downto 0);
		gdphs_wbs_readdata               : out std_logic_vector(15 downto 0);
		gdphs_wbs_writedata              : in  std_logic_vector(15 downto 0);
		gdphs_wbs_ack                    : out std_logic;
		gdphs_wbs_strobe                 : in  std_logic;
		gdphs_wbs_cycle                  : in  std_logic;
		gdphs_wbs_write                  : in  std_logic;
		gdphs_wbs_sel           		: in std_logic_vector(1 downto 0);   -- byte lane selct signals (16 bit bus)
  
       --------------------------
       -- Interrupt
       --------------------------
 
       gdphs_int	 : out std_ulogic;
	   
	   is68000_o 	 : out std_ulogic;	
	   
	   gdphs_cs_vector		  : in std_logic_vector(num_cs_signals_c-1 downto 0);
       --------------------------
       -- UART Receiver
       --------------------------
       RxD_i    : in  std_ulogic;
       TxD_o    : out std_ulogic;
       RTS_o    : out std_ulogic;
       CTS_i    : in  std_ulogic;
       --------------------------
       -- PS/2 Keyboard signals
       --------------------------
       -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
       -- PS/2 bus. In all other cases an input would be sufficient.
       Ps2Clk_io    : inout std_logic;
       -- PS/2 data line. Bidirectional for reading and writing data.
       Ps2Dat_io    : inout std_logic;
       --------------------------
       -- PS/2 Mouse signals
       --------------------------
       -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
       -- PS/2 bus. In all other cases an input would be sufficient.
       Ps2MouseClk_io    : inout std_logic;
       -- PS/2 data line. Bidirectional for reading and writing data.
       Ps2MouseDat_io    : inout std_logic;
       --------------------------
       -- Audio DAC-PWM out
       -- This DAC requires an external RC low-pass filter:
       --
       --   pwm_out 0---XXXXX---+---0 analog audio
       --                3k3    |
       --                      === 4n7
       --                       |
       --                      GND
       --------------------------
       PWM_OUT_o   : out std_ulogic;
       --------------------------
       -- Video out
       --------------------------
       Red_o      : out std_ulogic_vector(2 downto 0);
       Green_o    : out std_ulogic_vector(2 downto 0);
       Blue_o     : out std_ulogic_vector(2 downto 0);
       Hsync_o    : out std_ulogic;
       Vsync_o    : out std_ulogic;
       --------------------------
       -- SPI-Signals						-- TH: SD-Card
       --------------------------
       SD_SCK_o  : out std_ulogic;
       SD_nCS_o  : out std_ulogic_vector(1 downto 0);
       SD_MOSI_o : out std_ulogic;
       SD_MISO_i : in  std_ulogic;      
	   
	   
       --------------------------
       -- Video-Memory data bus				
       --------------------------       	 	  
		
		SRAM_nCS0    : out std_logic;
       	SRAM_nCS1   : out std_logic;       
	   	SRAM_ADDR   : out std_logic_vector(18 downto 0);	
       	SRAM_DB     : inout std_logic_vector(15 downto 0);
       	SRAM_nWR    : out std_logic;
       	SRAM_nOE    : out std_logic;
	   	SRAM_nBHE	: out std_logic;						
	   	SRAM_nBLE	: out std_logic;
				
	   --------------------------
       -- Monitoring (Debug) signals
       --------------------------
       monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)	   

	);
	end component;
	
---- gide module ---------------------------------
	component gide is
	port(
			--------------------------
	   		-- Data Bus (WishBone)
	   		--------------------------           	
			gide_reset_n					: in  std_logic;
			gide_clk						: in  std_logic;
		
			gide_wbs_address                : in  std_logic_vector(7 downto 0);
			gide_wbs_readdata               : out std_logic_vector(7 downto 0);
			gide_wbs_writedata              : in  std_logic_vector(7 downto 0);
			gide_wbs_ack                    : out std_logic;
			gide_wbs_strobe                 : in  std_logic;
			gide_wbs_cycle                  : in  std_logic;
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
	end component;
	

	-- component signal synchronizer	
	component InputSync
    generic(levels_g     : natural :=2;
            ResetValue_g : std_ulogic := '0');
    port (
      Input : in  std_ulogic;
      clk   : in  std_ulogic;
      clr_n : in  std_ulogic;
      q     : out std_ulogic);
  	end component;
  	
-- define constants
	constant GIDE_BASE_ADDR_c   : std_logic_vector(7 downto 0) := X"10"; -- r/w
	constant GDP_BASE_ADDR_c    : std_logic_vector(7 downto 0) := X"70"; -- r/w
	constant SFR_BASE_ADDR_c    : std_logic_vector(7 downto 0) := X"60"; -- w  
	constant COL_BASE_c         : std_logic_vector(7 downto 0) := X"A0"; -- r/w  
	constant CLUT_BASE_c        : std_logic_vector(7 downto 0) := X"A4"; -- r/w 
	constant KEY_BASE_ADDR_c    : std_logic_vector(7 downto 0) := X"68"; -- r  
	constant DIP_BASE_ADDR_c    : std_logic_vector(7 downto 0) := X"69"; -- r  
	constant MOUSE_BASE_ADDR_c  : std_logic_vector(7 downto 0) := X"88"; -- r/w  
	constant SER_BASE_ADDR_c    : std_logic_vector(7 downto 0) := X"F0"; -- r/w  
	constant SOUND_BASE_ADDR_c  : std_logic_vector(7 downto 0) := X"50"; -- r/w  
	constant SPI_BASE_ADDR_c    : std_logic_vector(7 downto 0) := X"00"; -- r/w 
	constant T1_BASE_ADDR_c     : std_logic_vector(7 downto 0) := X"F4"; -- r/w 
	constant VDIP_BASE_ADDR_c   : std_logic_vector(7 downto 0) := X"20"; -- r/w 
	constant KEY_OPTS_ADDR_c    : std_logic_vector(7 downto 0) := X"67"; -- r/w     
-- define signals

	-- commmon
	signal gdphs_reset_n			: std_logic;
	signal gdphs_clk				: std_logic;
	signal gdphs_int				: std_logic;
	-- slave
	signal gdphs_wbs_address    : std_logic_vector(19 downto 0);  -- Address bus
	signal gdphs_wbs_readdata   : std_logic_vector(15 downto 0);  -- Data bus for read access
	signal gdphs_wbs_writedata  : std_logic_vector(15 downto 0);  -- Data bus for write access
	signal gdphs_wbs_strobe     : std_logic;                      -- Data Strobe
	signal gdphs_wbs_write      : std_logic;                      -- Write access
	signal gdphs_wbs_ack        : std_logic;                      -- acknowledge
	signal gdphs_wbs_cycle      : std_logic;                      -- bus cycle in progress
	signal gdphs_wbs_sel		 : std_logic_vector(1 downto 0);   -- byte lane selct signals (16 bit bus)
	-- master	
    signal gdphs_wbm_address       : std_logic_vector(18 downto 0);
    signal gdphs_wbm_readdata      : std_logic_vector(15 downto 0);
    signal gdphs_wbm_writedata     : std_logic_vector(15 downto 0);
    signal gdphs_wbm_strobe        : std_logic;
	signal gdphs_wbm_sel		   : std_logic_vector(1 downto 0);   -- byte lane selct signals (16 bit bus)
    signal gdphs_wbm_write         : std_logic;
    signal gdphs_wbm_ack           : std_logic;
    signal gdphs_wbm_cycle         : std_logic;
	
	signal hsync : std_logic;
	signal vsync : std_logic;
		
	
	-- signals to gide modul	
	
	signal gide_wbs_address    : std_logic_vector(7 downto 0);  -- Address bus
	signal gide_wbs_readdata   : std_logic_vector(7 downto 0);  -- Data bus for read access
	signal gide_wbs_writedata  : std_logic_vector(7 downto 0);  -- Data bus for write access
	signal gide_wbs_strobe     : std_logic;                      -- Data Strobe
	signal gide_wbs_write      : std_logic;                      -- Write access
	signal gide_wbs_ack        : std_logic;                      -- acknowledge
	signal gide_wbs_cycle      : std_logic;                      -- bus cycle in progress
		
	signal is68000 	 : std_logic;	
	
	signal debug_sig : std_logic;
	
	
	
	-- wb signals generated in this module
	signal wbm_address    : std_logic_vector(19 downto 0);  -- Address bus (isMEMRQ,a18...a0)
    signal wbm_readdata   : std_logic_vector(15 downto 0);  -- Data bus for read access
    signal wbm_writedata  : std_logic_vector(15 downto 0);  -- Data bus for write access
    signal wbm_strobe     : std_logic;                      -- Data Strobe
	signal wbm_sel		  : std_logic_vector(1 downto 0);   -- byte lane selct signals (16 bit bus)
    signal wbm_write      : std_logic;                      -- Write access
    signal wbm_ack        : std_logic;                      -- acknowledge
    signal wbm_cycle      : std_logic;                       -- bus cycle in progress
	
	signal cs_vector		: std_logic_vector(num_cs_signals_c-1 downto 0);	-- cs Signals generated in this module
	

	
	signal write      : std_logic;
    signal read       : std_logic;
    signal strobe     : std_logic;
    signal writedata  : std_logic_vector(15 downto 0);
    signal address    : std_logic_vector(18 downto 0);
	
	signal fpga_en	   	: std_logic;
	signal sram_cs		: std_logic;
	signal gide_cs	  	: std_logic;
	
	signal gdphs_cs                     : std_logic;
    signal gdp_cs                       : std_logic;
	signal gdp_en                       : std_logic;
	signal sfr_cs                       : std_logic;
	signal col_cs                       : std_logic;
	signal clut_cs                       : std_logic;
	signal key_cs                       : std_logic;
	signal dip_cs                       : std_logic;
	signal mouse_cs                       : std_logic;
	signal ser_cs                       : std_logic;
	signal sound_cs                       : std_logic;
	signal spi_cs                       : std_logic;
	signal t1_cs                       : std_logic;
	signal vdip_cs                       : std_logic;
	signal kopts_cs                       : std_logic;
	
	signal s_fpga_en	   	: std_logic;
	signal s_gdphs_cs	   	: std_logic;
	signal s_sram_cs		: std_logic;
	signal s_gide_cs	  	: std_logic;
    signal s_gdp_cs       : std_logic;
	signal s_gdp_en       : std_logic;
	signal s_sfr_cs       : std_logic;
	signal s_col_cs       : std_logic;
	signal s_clut_cs      : std_logic;
	signal s_key_cs       : std_logic;
	signal s_dip_cs       : std_logic;
	signal s_mouse_cs     : std_logic;
	signal s_ser_cs       : std_logic;
	signal s_sound_cs     : std_logic;
	signal s_spi_cs       : std_logic;
	signal s_t1_cs        : std_logic;
	signal s_vdip_cs      : std_logic;
	signal s_kopts_cs     : std_logic;
	
	signal nIORQ,nIORQ_d		: std_logic;
	signal nMEMRQ,nMEMRQ_d		: std_logic;
	signal nWR,nRD		: std_logic;
	signal nReset		: std_logic;

	
	type ack_state_t is(ack_idle_e,ack_run_e,ack_eot_e);
	signal ack_state : ack_state_t;
	
	
	signal nkc_nWait_s : std_logic;
	


begin
-- define behaviour
	
	Hsync_o <= hsync;
	Vsync_o <= vsync;
									
	----------------------------------------------------------------------------
    -- bus signal syncronization
	----------------------------------------------------------------------------
	RESET : InputSync
 	generic map (
   	ResetValue_g => '0',
		levels_g => 2
 	)
 	port map (
     Input => reset_n_i,
     clk   => clk_i,
     clr_n => '1',
     q     => nReset );

      
	ISWR : InputSync
  	generic map (
    	ResetValue_g => '1',
		levels_g => 2
  	)
  	port map (
      Input => nkc_nWR_i,
      clk   => clk_i,
      clr_n => nReset,
      q     => nWR);
      
    ISRD : InputSync
  	generic map (
    	ResetValue_g => '1',
		levels_g => 2
  	)
  	port map (
      Input => nkc_nRD_i,
      clk   => clk_i,
      clr_n => nReset,
      q     => nRD); 
      
	  
	----  
    ISIORQ : InputSync
  	generic map (
    	ResetValue_g => '1',
		levels_g => 2
  	)
  	port map (
      Input => nkc_nIORQ_i,
      clk   => clk_i,
      clr_n => nReset,
      q     => nIORQ);
      
    ISMEMRQ : InputSync
  	generic map (
    	ResetValue_g => '1',
		levels_g => 2
  	)
  	port map (
      Input => nkc_nMEMRQ_i,
      clk   => clk_i,
      clr_n => nReset,
      q     => nMEMRQ); 
      
      
    -- ----------------------------------------------------------------------------
    --  External signals synchronization process
    -- ----------------------------------------------------------------------------
	
	-- check if we are selected...
	-- diese Chip-Select Signale müssen im Modul gdp_intercon nochmal generiert werden, evtl. kann man die durchschleifen	
	-- oder beide Module kombinieren. 
	
	gide_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 4) = GIDE_BASE_ADDR_c(7 downto 4) else '0';
	sram_cs <= not nkc_nMEMRQ_i	; -- Memory access is already decoded by xilinx cpld	(sramsize = 0x100000 = 1MB at 0x70.0000*cpu, z.Z. wird ein 4MB Bereich dekodiert ! )			
	gdp_cs <= not nkc_nIORQ_i  when  nkc_ADDR_i(7 downto 4) = GDP_BASE_ADDR_c(7 downto 4)  or  -- GDP
                                          (nkc_ADDR_i(7 downto 1) = SFR_BASE_ADDR_c(7 downto 1)) or
                                          (nkc_ADDR_i(7 downto 1) = COL_BASE_c(7 downto 1) and color_support_c) or -- SFRs
                                          (nkc_ADDR_i(7 downto 2) = CLUT_BASE_c(7 downto 2) and color_support_c) else
										'0';
    gdp_en <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 4) = GDP_BASE_ADDR_c(7 downto 4) else '0';
	sfr_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 1) = SFR_BASE_ADDR_c(7 downto 1) else '0';
	col_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 1) =  COL_BASE_c(7 downto 1)else '0';
	clut_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 2) =  CLUT_BASE_c(7 downto 2) else '0';
	key_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 0) =  KEY_BASE_ADDR_c else '0';
	dip_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 0) =  DIP_BASE_ADDR_c  else '0';
	mouse_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 3) =  MOUSE_BASE_ADDR_c(7 downto 3) else '0';
	ser_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 2) =  SER_BASE_ADDR_c(7 downto 2) else '0';
	sound_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 1) =  SOUND_BASE_ADDR_c(7 downto 1) else '0';
	spi_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 1) =  SPI_BASE_ADDR_c(7 downto 1) else '0';
	t1_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 2) =  T1_BASE_ADDR_c(7 downto 2)else '0';
	--vdip_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 2) =  VDIP_BASE_ADDR_c(7 downto 2) else '0';
	vdip_cs <= '0';
	kopts_cs <= not nkc_nIORQ_i when nkc_ADDR_i(7 downto 0) =  KEY_OPTS_ADDR_c else '0';
	
	gdphs_cs <= gdp_cs or gdp_en or sfr_cs or col_cs or clut_cs or key_cs or               
				dip_cs or mouse_cs or ser_cs or sound_cs or spi_cs or t1_cs or 
 				vdip_cs or kopts_cs;

	
	fpga_en <= gide_cs or sram_cs or gdphs_cs;
	
	----------------------------------------------------------------------------
	-- address decoding (synchronized )
	-- -------------------------------------------------------------------------
	
	s_gide_cs <= not nIORQ when wbm_address(7 downto 4) = GIDE_BASE_ADDR_c(7 downto 4) else '0';
	
	s_sram_cs <= not nMEMRQ	; -- Memory access is already decoded by xilinx cpld	(sramsize = 0x100000 = 1MB at 0x70.0000*cpu, z.Z. wird ein 4MB Bereich dekodiert ! )			
	s_gdp_cs <= not nIORQ  when  wbm_address(7 downto 4) = GDP_BASE_ADDR_c(7 downto 4)  or  -- GDP
                                          (nkc_ADDR_i(7 downto 1) = SFR_BASE_ADDR_c(7 downto 1)) or
                                          (nkc_ADDR_i(7 downto 1) = COL_BASE_c(7 downto 1) and color_support_c) or -- SFRs
                                          (nkc_ADDR_i(7 downto 2) = CLUT_BASE_c(7 downto 2) and color_support_c) else
											'0';
    s_gdp_en <= not nIORQ when wbm_address(7 downto 4) = GDP_BASE_ADDR_c(7 downto 4) else '0';
	s_sfr_cs <= not nIORQ when wbm_address(7 downto 1) = SFR_BASE_ADDR_c(7 downto 1) else '0';
	s_col_cs <= not nIORQ when wbm_address(7 downto 1) =  COL_BASE_c(7 downto 1)else '0';
	s_clut_cs <= not nIORQ when wbm_address(7 downto 2) =  CLUT_BASE_c(7 downto 2) else '0';
	s_key_cs <= not nIORQ when wbm_address(7 downto 0) =  KEY_BASE_ADDR_c else '0';
	s_dip_cs <= not nIORQ when wbm_address(7 downto 0) =  DIP_BASE_ADDR_c  else '0';
	s_mouse_cs <= not nIORQ when wbm_address(7 downto 3) =  MOUSE_BASE_ADDR_c(7 downto 3) else '0';
	s_ser_cs <= not nIORQ when wbm_address(7 downto 2) =  SER_BASE_ADDR_c(7 downto 2) else '0';
	s_sound_cs <= not nIORQ when wbm_address(7 downto 1) =  SOUND_BASE_ADDR_c(7 downto 1) else '0';
	s_spi_cs <= not nIORQ when wbm_address(7 downto 1) =  SPI_BASE_ADDR_c(7 downto 1) else '0';
	s_t1_cs <= not nIORQ when wbm_address(7 downto 2) =  T1_BASE_ADDR_c(7 downto 2)else '0';
	s_vdip_cs <= not nIORQ when nkc_ADDR_i(7 downto 2) =  VDIP_BASE_ADDR_c(7 downto 2) else '0';
	--s_vdip_cs <= '0';
	s_kopts_cs <= not nIORQ when wbm_address(7 downto 0) =  KEY_OPTS_ADDR_c else '0';
	
	s_gdphs_cs <= s_gdp_cs or s_gdp_en or s_sfr_cs or s_col_cs or s_clut_cs or s_key_cs or               
				s_dip_cs or s_mouse_cs or s_ser_cs or s_sound_cs or s_spi_cs or s_t1_cs or 
 				s_kopts_cs or s_sram_cs; --or s_vdip_cs;

	s_fpga_en <= s_gide_cs or s_gdphs_cs;
						
                    -- 16       -- 15       -- 14      -- 13      -- 12     -- 11       -- 10      -- 9        -- 8       -- 7       -- 6         -- 5       -- 4         -- 3       -- 2      -- 1        -- 0
	cs_vector <= s_sram_cs & s_gide_cs & s_gdphs_cs & s_gdp_cs & s_gdp_en & s_sfr_cs & s_col_cs & s_clut_cs & s_key_cs & s_dip_cs & s_mouse_cs & s_ser_cs & s_sound_cs & s_spi_cs & s_t1_cs & s_vdip_cs & s_kopts_cs;
	
	----------------------------------------------------------------------------
	-- sample bus signals (synchronized )
	-- -------------------------------------------------------------------------
	
	SYNC: process(clk_i,is68000,UDS_SIZ0_i, LDS_SIZ1_i,nkc_ADDR_i_020_A0,nkc_ADDR_i_020_A1)
	begin
	 if(nReset = '0') then
	    	nIORQ_d <= '0';
	    	nMEMRQ_d <= '0';
	    	
	    	wbm_strobe <= '0';
			wbm_cycle <= '0';
			wbm_write <= '0';	
	    	
	 elsif(rising_edge(clk_i)) then
	   nIORQ_d <= nIORQ;
	   nMEMRQ_d <= nMEMRQ;
	   
	   if(nIORQ = '0' or nMEMRQ = '0') then
	     
	     if( (nIORQ = '0' and nIORQ_d = '1') or (nMEMRQ = '0' and nMEMRQ_d = '1')) then  -- IORQ/MEMRQ has falling edge... 
	  
			   if( is68000 = '0') then 
			      -- 68020 bus interface --------------------------------------------------------
					if(	-- 16 bit bus mode CPU(31..24) == GDPRAM(7..0) -> BLE -> wbm_sel(0)
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0') or
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or -- (1)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0') or -- (2)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or -- (3)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0') or -- (4)
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or -- (5)
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0')    -- (6)
						) 
					then											
						wbm_sel(0) <= '1';
					else
						wbm_sel(0) <= '0';
					end if;
														
					if( -- 16 bit bus mode CPU(23..16) == GDPRAM(15..8) -> BHE -> wbm_sel(1)
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '1') or
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '1') or
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or -- (1)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '1') or
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0') or -- (2)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '1') or						
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or -- (3)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '1') or
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0') or -- (4)
										(LDS_SIZ1_i = '1' and UDS_SIZ0_i = '1' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '1') or
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '0') or -- (5)
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '0' and nkc_ADDR_i_020_A0 = '1') or
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '0') or -- (6)
										(LDS_SIZ1_i = '0' and UDS_SIZ0_i = '0' and nkc_ADDR_i_020_A1 = '1' and nkc_ADDR_i_020_A0 = '1') 
						)
					then
						wbm_sel(1) <= '1';
					else
						wbm_sel(1) <= '0';
					end if;	
					
					if(sram_cs = '1') then  -- 68020 access to sram (16Bit)
						wbm_address(18 downto 1)    <= nkc_ADDR_i(17 downto 0);
						wbm_address(0) <= nkc_ADDR_i_020_A1;
					else					-- 68020 access to IO (32bit)	
						wbm_address(18 downto 0)    <= nkc_ADDR_i(18 downto 0);	
					end if;
									
			   else 
			     -- 68000 bus interface ---------------------------------------------------------------------------------
					 if(UDS_SIZ0_i = '1') then wbm_sel(0) <= '1'; else wbm_sel(0) <= '0'; end if;				
					 if(LDS_SIZ1_i = '1') then wbm_sel(1) <= '1'; else wbm_sel(1) <= '0'; end if;
					 
					 wbm_address(18 downto 0)    <= nkc_ADDR_i(18 downto 0); -- 32 Bit Zugriff auf 8Bit NKC IO Bereich oder MEM 16Bit mit 68000
					 										
			   end if;	--  is68000 = '0'
			   
			   wbm_address(19) <= not nkc_nMEMRQ_i; -- A19 = 1 if MemAccess ! 			
			   wbm_writedata  <= nkc_DB;			-- sample data
			   
		      end if;	 -- nIORQ_d/nMEMRQ_d = '1'
		      
		      if(fpga_en = '1') then
		       	wbm_strobe  <= not (nWR and nRD);	
		       	wbm_cycle  <= not (nWR and nRD);	
				wbm_write   <= not nWR;	
			  else
			    wbm_strobe <= '0';
			    wbm_cycle <= '0';
				wbm_write <= '0';	
		      end if;
		   
		   else
		   		wbm_strobe <= '0';
				wbm_write <= '0';   
		   end if; -- nIORQ_d/nMEMRQ_d = '0'   
		   
		end if; -- rising_edge( clk_i )       
    end process;
	
	  
	nkc_DB <= wbm_readdata when ((nRD = '0') and (fpga_en = '1')) else	              
			  (others => 'Z'); 		


	-- Wait Signal Generation
	--nkc_nWAIT_o	<= '1'; 
	nkc_nWait_o <= nkc_nWait_s;
	process( nReset, clk_i, strobe)
	begin
		if(nReset='0') then						
			nkc_nWait_s <= 'Z';	
		  --elsif( sram_cs = '1') then -- TEST
		    --nkc_nWait_s <= '0';
		  --elsif( strobe = '1') then	-- strobe kommt erst mit der steigende Flanke von clk, das ist zu spät !!		
		  elsif( fpga_en = '1') then
		    nkc_nWait_s <= wbm_ack;	-- generate cpu wait signal																
		  else            
			nkc_nWait_s <= 'Z';
          end if;		
	 end process;
	
	-- driver
	driver_nEN_o <= not nReset; --not(output_en and (not nkc_nWR_i or not nkc_nRD_i)); 
    driver_DIR_o <= '1' when (fpga_en and not nkc_nRD_i)='1' else '0';
      
	----------------------------------------------------------------------------
	-- interrupt routing
	-- -------------------------------------------------------------------------
   
    nIRQ_o      <= '0' when (gdphs_int = '1') else '1';	  
	--nNMI_o	 	<= '1'; 	
	--IRQ0_o	 	<= '0'; 
	--IRQ1_o	 	<= '0';	
	
	----------------------------------------------------------------------------
	-- address decoding
	-- -------------------------------------------------------------------------
	 
	 gdphs_wbs_address <= wbm_address;
	 gide_wbs_address <= wbm_address(7 downto 0);

	 -----------------------------
     -- Control signals to slave
     -----------------------------
	
     gdphs_wbs_strobe <= (wbm_strobe and s_gdphs_cs );
	 gdphs_wbs_cycle  <= (wbm_cycle and s_gdphs_cs  );
	 gdphs_wbs_write  <= (wbm_write and s_gdphs_cs  );
	 gdphs_wbs_sel    <= wbm_sel;-- when gdphs_cs='1' else (others=> '0');
	 gdphs_wbs_writedata  <= wbm_writedata;-- when (wrapper_wbm_write and gdphs_cs) = '1' else (others => '0');
	 
	 gide_wbs_strobe <= (wbm_strobe and s_gide_cs );
	 gide_wbs_cycle  <= (wbm_cycle and s_gide_cs  );
	 gide_wbs_write  <= (wbm_write and s_gide_cs  );
	 gide_wbs_writedata  <= wbm_writedata(7 downto 0);-- when (wrapper_wbm_write and gide_cs) = '1' else (others => '0');
	 
	 	 
     -------------------------------
     -- Control signal for master --
     -------------------------------
	 
     wbm_readdata <= gdphs_wbs_readdata when s_gdphs_cs='1' else
                             "00000000" & gide_wbs_readdata  when s_gide_cs='1' else							 
                                        (others => '0');
	 
										
     wbm_ack <=  (gdphs_wbs_ack and s_gdphs_cs)  or  (gide_wbs_ack and s_gide_cs);
						 
	 
     

--------------------------------------------------------------------------------------------
-- gdphs Module: A. Voggeneder  ************************************************************
--------------------------------------------------------------------------------------------

		
		-- gdphs module von Andreas V.
		GDPHS: gdp_lattice_top
		port map(
		--------------------------
	   	-- Data Bus (WishBone)
	   	--------------------------           	
		gdphs_reset_n					=> nReset,
		gdphs_clk						=> clk_i,
		
		gdphs_wbs_address                => gdphs_wbs_address,
		gdphs_wbs_readdata               => gdphs_wbs_readdata,
		gdphs_wbs_writedata              => gdphs_wbs_writedata,
		gdphs_wbs_ack                    => gdphs_wbs_ack,
		gdphs_wbs_strobe                 => gdphs_wbs_strobe,
		gdphs_wbs_cycle                  => gdphs_wbs_cycle,
		gdphs_wbs_write                  => gdphs_wbs_write,
		gdphs_wbs_sel		 			=> gdphs_wbs_sel,   -- byte lane selct signals (16 bit bus)
  
       --------------------------
       -- Interrupt
       --------------------------
 
       gdphs_int        => gdphs_int,
	   
	   
	   is68000_o 	 => is68000,	
	   gdphs_cs_vector => cs_vector,
       --------------------------
       -- UART Receiver
       --------------------------
       RxD_i    => RxD_i,
       TxD_o    => TxD_o,
       RTS_o    => RTS_o,
       CTS_i    => CTS_i,
       --------------------------
       -- PS/2 Keyboard signals
       --------------------------
       -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
       -- PS/2 bus. In all other cases an input would be sufficient.
       Ps2Clk_io    => Ps2Clk_io,
       -- PS/2 data line. Bidirectional for reading and writing data.
       Ps2Dat_io    => Ps2Dat_io,
       --------------------------
       -- PS/2 Mouse signals
       --------------------------
       -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
       -- PS/2 bus. In all other cases an input would be sufficient.
       Ps2MouseClk_io    => Ps2MouseClk_io,
       -- PS/2 data line. Bidirectional for reading and writing data.
       Ps2MouseDat_io    => Ps2MouseDat_io,
       --------------------------
       -- Audio DAC-PWM out
       -- This DAC requires an external RC low-pass filter:
       --
       --   pwm_out 0---XXXXX---+---0 analog audio
       --                3k3    |
       --                      === 4n7
       --                       |
       --                      GND
       --------------------------
       PWM_OUT_o   => PWM_OUT_o,
       --------------------------
       -- Video out
       --------------------------
       std_logic_vector(Red_o)      => Red_o,
       std_logic_vector(Green_o)    => Green_o,
       std_logic_vector(Blue_o)     => Blue_o,
       Hsync_o    => hsync,
       Vsync_o    => vsync,
       --------------------------
       -- SPI-Signals						
       --------------------------
       SD_SCK_o  => SD_SCK_o,
       std_logic_vector(SD_nCS_o)  => SD_nCS_o,
       SD_MOSI_o => SD_MOSI_o,
       SD_MISO_i => SD_MISO_i,
       --------------------------
       -- Video-Memory data bus				
       --------------------------       	 
	   
	   SRAM_nCS0   =>    SRAM_nCS0  ,
       SRAM_nCS1   =>    SRAM_nCS1  ,
	   SRAM_ADDR   =>    SRAM_ADDR  ,
       SRAM_DB     =>    SRAM_DB    ,
       SRAM_nWR    =>    SRAM_nWR   ,
       SRAM_nOE    =>    SRAM_nOE   ,
	   SRAM_nBHE   => 	 SRAM_nBHE	,
	   SRAM_nBLE   =>    SRAM_nBLE  ,
	   	   
	   ---------------------------
	   -- Debug
	   ---------------------------
	   monitoring_o(0)	=> IRQ0_o, -- DEBUG clk
	   monitoring_o(1)	=> IRQ1_o, -- DEBUG kernel_req
	   monitoring_o(2)  => nNMI_o  -- DEBUG read_req
	);
	
	GIDE1: gide
		port map(
		--------------------------
	   		-- Data Bus (WishBone)
	   		--------------------------           	
			gide_reset_n					=> nReset,
			gide_clk						=> clk_i,
		
			gide_wbs_address                => gide_wbs_address,
			gide_wbs_readdata               => gide_wbs_readdata,
			gide_wbs_writedata              => gide_wbs_writedata,
			gide_wbs_ack                    => gide_wbs_ack,
			gide_wbs_strobe                 => gide_wbs_strobe,
			gide_wbs_cycle                  => gide_wbs_cycle,
			gide_wbs_write                  => gide_wbs_write,
			
    		--------------------------
       		-- IDE Signals
       		--------------------------
    		IDE_D							=> IDE_D,
    		IDE_RD							=> IDE_RD,
            IDE_WR							=> IDE_WR,
            IDE_INT							=> IDE_INT,
           	IDE_CS0							=> IDE_CS0,
           	IDE_CS1							=> IDE_CS1,    	
           	IDE_IA							=> IDE_IA
	);
	


end rtl;