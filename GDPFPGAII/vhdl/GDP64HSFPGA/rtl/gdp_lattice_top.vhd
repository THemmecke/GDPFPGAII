--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - Toplevel for Lattice FPGA
-- File        : GDP_kernel.vhdrom_ena

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
use work.gdp_fpgaii_global.all;

use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity gdp_lattice_top is
  generic(sim_g      : boolean := false);
  port(
  
		 --------------------------
         -- Interrupt
         --------------------------
	   
		gdphs_int	 : out std_ulogic;
				
		is68000_o 	 : out std_ulogic;	-- signal to wrapper
		gdphs_cs_vector : in std_logic_vector(num_cs_signals_c-1 downto 0);

				
		--------------------------
	   	-- Data Bus (WishBone)
	   	--------------------------        
		
		-- Common WB Signals
		
		-- Slave WB Signals
		gdphs_reset_n					: in  std_logic;
		gdphs_clk						: in  std_logic;
				
		gdphs_wbs_address                : in  std_logic_vector(19 downto 0);
		gdphs_wbs_readdata               : out std_logic_vector(15 downto 0);
		gdphs_wbs_writedata              : in  std_logic_vector(15 downto 0);
		gdphs_wbs_ack                    : out std_logic;
		gdphs_wbs_strobe                 : in  std_logic;
		gdphs_wbs_cycle                  : in  std_logic;
		gdphs_wbs_write                  : in  std_logic;
		gdphs_wbs_sel					 : in std_logic_vector(1 downto 0);
		
		-- Master WB Signals
		
		-- Master Interface --
		
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
       -- Monitoring (Debug) signals
       --------------------------
       monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
       );
end gdp_lattice_top;

architecture rtl of gdp_lattice_top is

  component gdp_top is
    generic(INT_CHR_ROM_g : boolean := true); 
    port(reset_n_i     : in  std_ulogic;
         clk_i         : in  std_ulogic;
         clk_en_i      : in  std_ulogic;
         --------------------------
         -- internal data bus (Register)
         --------------------------
         Adr_i     : in  std_ulogic_vector(19 downto 0);
         gdp_en_i  : in  std_ulogic;
         sfr_en_i  : in  std_ulogic;
         col_en_i  : in  std_ulogic;
         clut_en_i : in  std_ulogic;
		 sram_cs_i : in  std_ulogic;
         DataIn_i  : in  std_ulogic_vector(15 downto 0);
		 DataSel_i : in  std_ulogic_vector(1 downto 0);
		 DataAck_o : out std_ulogic;
		 DataCycle_i : in std_ulogic;
         Rd_i      : in  std_ulogic;
         Wr_i      : in  std_ulogic;
         DataOut_o : out std_ulogic_vector(15 downto 0);
         --------------------------
         -- Video out
         --------------------------
--         pixel_o    : out std_ulogic;
         pixel_red_o   : out std_ulogic_vector(2 downto 0);
         pixel_green_o : out std_ulogic_vector(2 downto 0);
         pixel_blue_o  : out std_ulogic_vector(2 downto 0);
         Hsync_o       : out std_ulogic;
         Vsync_o       : out std_ulogic;
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
	   	
         ---------------------------
		 monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
         );
  end component;

  component InputSync
    generic(levels_g     : natural :=1;
            ResetValue_g : std_ulogic := '0');
    port (
      Input : in  std_ulogic;
      clk   : in  std_ulogic;
      clr_n : in  std_ulogic;
      q     : out std_ulogic);
  end component;
  
  component Receiver
    port (
      clk        : in  std_ulogic;
      clr_n      : in  std_ulogic;
      RxD        : in  std_ulogic;
      Busy       : in  std_ulogic;
      DoutPar    : out std_ulogic_vector(7 downto 0);
      DataValid  : out std_ulogic;
      ErrorFlags : out std_ulogic_vector(1 downto 0));
  end component;
  
  component PS2Keyboard
    port (
      reset_n_i : in    std_logic;
      clk_i     : in    std_logic;
      Ps2Clk_io : inout std_logic;
      Ps2Dat_io : inout std_logic;
      KeyCS_i   : in  std_ulogic;
      DipCS_i   : in  std_ulogic;
      KOptCS_i	: in  std_ulogic;	  -- TH
	  is68000_o : out std_ulogic;
      Rd_i      : in  std_ulogic;	
      Wr_i	: in  std_ulogic;
      DataIn_i	: in std_ulogic_vector(7 downto 0);
      DataOut_o : out std_ulogic_vector(7 downto 0);
	  
	  --------------------------
      -- Monitoring (Debug) signals
      --------------------------
      monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
    );
  end component;
  
  component PS2Mouse is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    Ps2Clk_io    : inout std_logic;
    Ps2Dat_io    : inout std_logic;
    Adr_i        : in  std_ulogic_vector(2 downto 0);
    en_i         : in  std_ulogic;
    DataIn_i     : in  std_ulogic_vector(7 downto 0);
    Rd_i         : in  std_ulogic;
    Wr_i         : in  std_ulogic;
    DataOut_o    : out std_ulogic_vector(7 downto 0)
  );
  end component;
  
  component Ser1
    port(
      reset_n_i    : in  std_logic;
      clk_i        : in  std_logic;
      RxD_i        : in  std_ulogic;
      TxD_o        : out std_ulogic;
      RTS_o        : out std_ulogic;
      CTS_i        : in  std_ulogic;
      DTR_o        : out std_ulogic;
      Adr_i        : in  std_ulogic_vector(1 downto 0);
      en_i         : in  std_ulogic;
      DataIn_i     : in  std_ulogic_vector(7 downto 0);
      Rd_i         : in  std_ulogic;
      Wr_i         : in  std_ulogic;
      DataOut_o    : out std_ulogic_vector(7 downto 0);
      Intr_o       : out std_ulogic;
	  --------------------------
	  -- Monitoring (Debug) signals
	  --------------------------
	  monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
    );
  end component;
  
  component WF2149IP_TOP_SOC is
    port(
      SYS_CLK     : in std_ulogic; -- Read the inforation in the header!
      RESETn      : in std_ulogic;
      WAV_CLK     : in std_ulogic; -- Read the inforation in the header!
      SELn        : in std_ulogic;

      BDIR        : in std_ulogic;
      BC2, BC1    : in std_ulogic;

      A9n, A8     : in std_ulogic;
      DA_IN       : in std_ulogic_vector(7 downto 0);
      DA_OUT      : out std_ulogic_vector(7 downto 0);
      DA_EN       : out std_ulogic;
      
      IO_A_IN     : in std_ulogic_vector(7 downto 0);
      IO_A_OUT    : out std_ulogic_vector(7 downto 0);
      IO_A_EN     : out std_ulogic;
      IO_B_IN     : in std_ulogic_vector(7 downto 0);
      IO_B_OUT    : out std_ulogic_vector(7 downto 0);
      IO_B_EN     : out std_ulogic;

--      OUT_A       : out std_ulogic; -- Analog (PWM) outputs.
--      OUT_B       : out std_ulogic;
--      OUT_C       : out std_ulogic
      PWM_OUT     : out std_ulogic
    );
  end component;
  
  component SPI_Interface is
    port(
      reset_n_i    : in  std_logic;
      clk_i        : in  std_logic;
      SD_SCK_o  : out std_ulogic;
      SD_nCS_o  : out std_ulogic_vector(1 downto 0);
      SD_MOSI_o : out std_ulogic;
      SD_MISO_i : in  std_ulogic;
      Adr_i     : in  std_ulogic_vector(0 downto 0);
      en_i      : in  std_ulogic;
      DataIn_i  : in  std_ulogic_vector(7 downto 0);
      Rd_i      : in  std_ulogic;
      Wr_i      : in  std_ulogic;
      DataOut_o : out std_ulogic_vector(7 downto 0)
    );
  end component;
  
  component Timer is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    Irq_o     : out std_ulogic;
    Adr_i     : in  std_ulogic_vector(1 downto 0);
    en_i      : in  std_ulogic;
    DataIn_i  : in  std_ulogic_vector(7 downto 0);
    Rd_i      : in  std_ulogic;
    Wr_i      : in  std_ulogic;
    DataOut_o : out std_ulogic_vector(7 downto 0)
  );
  end component;
  
  component SPI_VDIP is
    port(
      reset_n_i    : in  std_logic;
      clk_i        : in  std_logic;
      VDIP_SCK_o  : out std_ulogic;
      VDIP_CS_o   : out std_ulogic;
      VDIP_MOSI_o : out std_ulogic;
      VDIP_MISO_i : in  std_ulogic;
      Adr_i     : in  std_ulogic_vector(1 downto 0);
      en_i      : in  std_ulogic;
      DataIn_i  : in  std_ulogic_vector(7 downto 0);
     Rd_i      : in  std_ulogic;
      Wr_i      : in  std_ulogic;
      DataOut_o : out std_ulogic_vector(7 downto 0)
    );
  end component;
  
  constant use_ser_key_c   : boolean := false;
  constant use_ps2_key_c   : boolean := true;
  constant use_ps2_mouse_c : boolean := true;
  constant use_ser1_c      : boolean := true;
  constant use_sound_c     : boolean := true;
  constant use_spi_c       : boolean := true;
  constant use_timer_c     : boolean := true;
  constant use_vdip_c      : boolean := false;
  
  constant dipswitches_c   : std_logic_vector(7 downto 0) := X"6D"; 				-- GDP-FPGA, GDP-HS, IDE, UHR
--  constant dipswitches_c   : std_logic_vector(7 downto 0) := X"49";
--  constant dipswitches1_c : std_logic_vector(7 downto 0) := X"01";
-- Bedeutung der Bits:
--         1 0 0 1  = 0x09
--   1 0 0 1 0 0 1  = 0x49
--   1 1 0 1 0 0 1  = 0x69 (GDP-FPGA, GDP-HS, IDE)
--   0 1 0 1 0 0 1  = 0x29 (GDP-HS, IDE)
--   1 1 0 1 1 0 1  = 0x6D (GDP-FPGA, GDP-HS, IDE, UHR)	
-- 7 6 5 4 3 2 1 0   
-- | | | | | | | | Reserviert (?)
-- | | | | | | | Autoboot beim Starten des Rechners
-- | | | | | | Uhrenbaugruppe vorhanden
-- | | | | | GDP64HS vorhanden
-- | | | | SCSI-HARDDISK vorhanden
-- | | | IDE-Disk vorhanden
-- | | GDP-FPGA vorhanden
-- | Reserviert 

  
  signal reset_n           : std_ulogic;
  
  signal GDP_SRAM_ADDR     : std_ulogic_vector(18 downto 0);
  signal GDP_SRAM_datao    : std_ulogic_vector(15 downto 0);
  signal GDP_DataOut       : std_ulogic_vector(15 downto 0);
  signal GDP_SRAM_datai    : std_ulogic_vector(15 downto 0);
  signal GDP_SRAM_sel      : std_ulogic_vector(1 downto 0);
  signal GDP_SRAM_we       : std_ulogic;
  signal GDP_SRAM_cs	   : std_ulogic;
  
--  signal VGA_pixel         : std_ulogic;
  signal gdp_Rd_clk            : std_ulogic;
  signal gdp_cs, sram_cs   : std_ulogic;
  signal gdp_en,sfr_en     : std_ulogic;
  signal col_en,clut_en    : std_ulogic;
  signal gdp_ack		   : std_ulogic;

--  signal nIORQ,nIORQ_d     : std_ulogic;
--  signal nRD_d             : std_ulogic;
--  signal nWR_d             : std_ulogic;
  signal nWr,nRd           : std_ulogic;
--  signal IORQ              : std_ulogic;
--  signal Addr              : std_ulogic_vector(7 downto 0);
--  signal data_in           : std_ulogic_vector(7 downto 0);
  signal output_en,fpga_en : std_ulogic;
  signal key_cs,dip_cs,kopt_cs     : std_ulogic;																		
  signal mouse_cs          : std_ulogic;
  
  signal BusyRX              : std_ulogic;
  signal DoutParRX,key_data  : std_ulogic_vector(7 downto 0);
  signal DataValidRX         : std_ulogic;
  signal OldDataValidRX      : std_ulogic;
  signal gdp_base,sfr_base,key_base,dip_base,kopt_base : std_ulogic_vector(7 downto 0);        							
  signal dipsw             : std_logic_vector(7 downto 0);
  signal mouse_data        : std_ulogic_vector(7 downto 0);
  
  signal ser_cs            : std_ulogic;
  signal ser_data          : std_ulogic_vector(7 downto 0);
  
  signal snd_cs            : std_ulogic;
  signal snd_data          : std_ulogic_vector(7 downto 0); 
  signal snd_bdir,snd_bc1  : std_ulogic;
  signal wav_en            : std_ulogic;
  signal wav_cnt           : natural range 0 to 19; -- 2 MHz
  
  signal spi_cs            : std_ulogic;
  signal spi_data          : std_ulogic_vector(7 downto 0);
  signal vdip_cs           : std_ulogic;
  signal vdip_data         : std_ulogic_vector(7 downto 0);
  
  signal t1_cs,t1_irq      : std_ulogic;
  signal t1_data           : std_ulogic_vector(7 downto 0);
 
	constant shreg_length_c    : natural                                      := 10;
	signal shreg : std_ulogic_vector(shreg_length_c-1 downto 0);
  
  -- DEBUG
  signal q : std_logic_vector(10 downto 0);
  
  
  -- gdp_bi internal signals
--  signal nIORQ,nIORQ_d     : std_ulogic;
  signal nRD_d         : std_ulogic;
  signal nWR_d         : std_ulogic;
 
begin

--    wait_gen: process(gdphs_clk, shreg, gdphs_wbs_strobe,shreg)
--	begin
--	if gdphs_wbs_strobe = '0' then
--	  shreg <= (others => '0');  
--	elsif rising_edge(gdphs_clk) then
--        for i in 0 to shreg_length_c-2 loop shreg(i + 1) <= shreg(i); end loop;
--		shreg(0) <= '1';
--     end if;
--    end process wait_gen;
		
	--gdphs_wbs_ack <= gdphs_wbs_strobe when shreg(shreg_length_c-1) = '1' else '0'; -- make wishbone happy	
	gdphs_wbs_ack <= gdp_ack when gdp_en = '1' else
					 gdp_ack when sfr_en = '1' else
					 gdp_ack when col_en = '1' else
					 gdp_ack when clut_en = '1' else					 
					 gdp_ack when sram_cs = '1' else					 
					 gdphs_wbs_strobe;
	
	

  dipsw <= dipswitches_c;-- when addr_sel_i = '1' else
--           dipswitches1_c;


  reset_n  <= gdphs_reset_n;
  nWr <= not gdphs_wbs_write;
  nRD <= gdphs_wbs_write;
   
  process(gdphs_clk,reset_n)
  begin
    if reset_n = '0' then
      nRD_d        <= '1';
      gdp_Rd_clk         <= '0';
    elsif rising_edge(gdphs_clk) then
      nRD_d        <= nRD;
      gdp_Rd_clk    <= '0';
      if gdphs_wbs_strobe = '1' then

        if fpga_en = '1' or nRD = '0' then
          gdp_Rd_clk   <= not nRD and nRD_d; 					-- Rd_o geht für einen Clock-Cycle auf 1 (fallende Flanke von nRD detektiert)
        else
          nRD_d <= '1';
        end if;
      else
        nRD_d <= '1';
      end if;
    end if;
  end process;
  

  fpga_en      <= gdp_cs or key_cs or dip_cs or mouse_cs or ser_cs or 
                  snd_cs or spi_cs or t1_cs or vdip_cs or kopt_cs; 

  process(gdphs_clk,reset_n)   
  begin
    if reset_n = '0' then
      output_en <= '0';
    elsif rising_edge(gdphs_clk) then
      output_en <= fpga_en;
    end if;
  end process;

                   
  gdphs_wbs_readdata <=     
							std_logic_vector(GDP_DataOut) when (output_en and (gdp_cs or sram_cs))='1' else
							"00000000" & std_logic_vector(key_data)    when (output_en and key_cs   and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(key_data)    when (output_en and kopt_cs   and not gdphs_wbs_write)='1' else				-- TH: read keyboard options register                   		  
							"00000000" & dipsw                         when (output_en and dip_cs   and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(mouse_data)  when (output_en and mouse_cs and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(ser_data)    when (output_en and ser_cs   and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(snd_data)    when (output_en and snd_cs   and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(spi_data)    when (output_en and spi_cs   and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(t1_data)     when (output_en and t1_cs    and not gdphs_wbs_write)='1' else
							"00000000" & std_logic_vector(vdip_data)   when (output_en and vdip_cs  and not gdphs_wbs_write)='1' else	                
								(others => 'Z') after 1 ns;
  
      
  GDP: gdp_top
    port map (
      reset_n_i   => reset_n,
      clk_i       => gdphs_clk,
      clk_en_i    => '1',
	  Adr_i 		=> std_ulogic_vector(gdphs_wbs_address),
      gdp_en_i    => gdp_en,
      sfr_en_i    => sfr_en,
      col_en_i    => col_en,
      clut_en_i   => clut_en,
	  sram_cs_i	=> sram_cs,
	  DataIn_i    => std_ulogic_vector(gdphs_wbs_writedata),
	  DataSel_i   => std_ulogic_vector(gdphs_wbs_sel),
	  DataAck_o   => gdp_ack,	  
	  DataCycle_i => std_ulogic(gdphs_wbs_cycle),
      Rd_i        => gdp_Rd_clk,
      Wr_i        => gdphs_wbs_write,
      DataOut_o   => GDP_DataOut,
      pixel_red_o   => red_o,
      pixel_green_o => green_o,
      pixel_blue_o  => blue_o,
      Hsync_o     => Hsync_o,
      Vsync_o     => Vsync_o,
	 
	  
	  SRAM_nCS0   =>    SRAM_nCS0  ,
      SRAM_nCS1   =>    SRAM_nCS1  ,
	  SRAM_ADDR   =>    SRAM_ADDR  ,
      SRAM_DB     =>    SRAM_DB    ,
      SRAM_nWR    =>    SRAM_nWR   ,
      SRAM_nOE    =>    SRAM_nOE   ,
	  SRAM_nBHE   => 	 SRAM_nBHE	,
	  SRAM_nBLE   =>    SRAM_nBLE  ,
	  
	  monitoring_o => monitoring_o
      );
  
                
	
	--cs_vector <= sram_cs(16) & gide_cs(15) & 
	--			 gdphs_cs(14) & gdp_cs(13) & gdp_en(12) & sfr_cs(11) & col_cs(10) & clut_cs(9) & key_cs(8) & dip_cs(7) & mouse_cs(6) & 
	--           ser_cs(5) & sound_cs(4) & spi_cs(3) & t1_cs(2) & vdip_cs(1) & kopts_cs(0);
	sram_cs <= gdphs_cs_vector(16)  when gdphs_wbs_cycle = '1' else '0'; 
	
	gdp_cs  <= gdphs_cs_vector(13) when gdphs_wbs_cycle = '1' else '0';  
    gdp_en  <= gdphs_cs_vector(12) when gdphs_wbs_cycle = '1' else '0';
    sfr_en  <= gdphs_cs_vector(11) when gdphs_wbs_cycle = '1' else '0';
    col_en  <= gdphs_cs_vector(10) when gdphs_wbs_cycle = '1' else '0';
    clut_en <= gdphs_cs_vector(9)  when gdphs_wbs_cycle = '1' else '0';
	key_cs  <= gdphs_cs_vector(8)  when gdphs_wbs_cycle = '1' else '0';
  	dip_cs  <= gdphs_cs_vector(7)  when gdphs_wbs_cycle = '1' else '0';
  	kopt_cs <= gdphs_cs_vector(0)  when gdphs_wbs_cycle = '1' else '0';
	--
	mouse_cs<= gdphs_cs_vector(6)  when gdphs_wbs_cycle = '1' else '0';
	ser_cs 	<= gdphs_cs_vector(5)  when gdphs_wbs_cycle = '1' else '0';
	snd_cs 	<= gdphs_cs_vector(4)  when gdphs_wbs_cycle = '1' else '0';
	spi_cs 	<= gdphs_cs_vector(3)  when gdphs_wbs_cycle = '1' else '0';
	t1_cs 	<= gdphs_cs_vector(2)  when gdphs_wbs_cycle = '1' else '0';    
	

	
  no_key1: if not use_ser_key_c and not use_ps2_key_c generate
    DoutParRX <= (others =>'0');
    BusyRX    <= '1';
    key_data  <= not BusyRX & DoutParRX(6 downto 0);
  end generate;
  
  impl_key2: if use_ps2_key_c generate
    kbd: PS2Keyboard
      port map (
       reset_n_i => reset_n,
       clk_i     => gdphs_clk,
       Ps2Clk_io => Ps2Clk_io,
       Ps2Dat_io => Ps2Dat_io,
       KeyCS_i   => key_cs,
       DipCS_i   => dip_cs,
       KOptCS_i  => kopt_cs,
	   is68000_o => is68000_o,
       Rd_i      => gdp_Rd_clk,
       Wr_i	  => gdphs_wbs_write,
  	   DataIn_i  => std_ulogic_vector(gdphs_wbs_writedata(7 downto 0)),
       DataOut_o => key_data,	 
       monitoring_o => open
    );
   end generate;
  

  impl_mouse: if use_ps2_mouse_c generate 
      --mouse_cs <= IORQ when Addr(7 downto 3)=MOUSE_BASE_ADDR_c(7 downto 3) else
                --'0';
    mouse : PS2Mouse
      port map (
        reset_n_i    => reset_n,
        clk_i        => gdphs_clk,
        Ps2Clk_io    => Ps2MouseClk_io,
        Ps2Dat_io    => Ps2MouseDat_io,
        Adr_i        => std_ulogic_vector(gdphs_wbs_address(2 downto 0)),
        en_i         => mouse_cs,
        DataIn_i     => std_ulogic_vector(gdphs_wbs_writedata(7 downto 0)),
        Rd_i         => gdp_Rd_clk,
        Wr_i         => gdphs_wbs_write,
        DataOut_o    => mouse_data
      );
  end generate;
  
  no_mouse: if not use_ps2_mouse_c generate
    mouse_data     <= (others =>'0');
    --mouse_cs       <= '0';
    Ps2MouseClk_io <= 'Z';
    Ps2MouseDat_io <= 'Z';
  end generate;
  
  impl_ser1: if use_ser1_c generate 
    --ser_cs <= IORQ when Addr(7 downto 2)=SER_BASE_ADDR_c(7 downto 2) else -- 0xF0 - 0xF3
              --'0';
    
    ser : Ser1
      port map (
        reset_n_i   => reset_n,
        clk_i       => gdphs_clk,
        RxD_i       => RxD_i,
        TxD_o       => TxD_o,
        RTS_o       => RTS_o,
        CTS_i       => CTS_i,
        DTR_o       => open,
        Adr_i       => std_ulogic_vector(gdphs_wbs_address(1 downto 0)),
        en_i        => ser_cs,
        DataIn_i    => std_ulogic_vector(gdphs_wbs_writedata(7 downto 0)),
        Rd_i        => gdp_Rd_clk,
        Wr_i        => gdphs_wbs_write,
        DataOut_o   => ser_data,
        Intr_o      => open,
		monitoring_o => open
      );
  end generate;
  no_ser1: if not use_ser1_c generate
    ser_data       <= (others =>'0');
    --ser_cs         <= '0';
    RTS_o          <= CTS_i;
    TxD_o          <= RxD_i;
  end generate;      
     
  impl_sound : if use_sound_c generate
    --snd_cs <= IORQ when Addr(7 downto 1)=SOUND_BASE_ADDR_c(7 downto 1) else -- 0x50 - 0x51
              --'0';
    snd_bdir <= snd_cs and gdphs_wbs_write;
    snd_bc1  <= snd_cs and (gdp_Rd_clk or (gdphs_wbs_write and not gdphs_wbs_address(0))); --(not snd_cs) nor Addr(0);    

    process(gdphs_clk,reset_n)
    begin
      if reset_n = '0' then
        wav_cnt <= 0;
        wav_en  <= '0';
      elsif rising_edge(gdphs_clk) then
        wav_en  <= '0';
        if wav_cnt < 19 then
          wav_cnt <= wav_cnt +1;
        else
          wav_cnt <= 0;
          wav_en  <= '1';
        end if;
      end if;
    end process;
    
    Sound_inst : WF2149IP_TOP_SOC
      port map (
        SYS_CLK   => gdphs_clk,
        RESETn    => reset_n,
        WAV_CLK   => wav_en,
        SELn      => '1',
        BDIR      => snd_bdir,
        BC2       => '1',
        BC1       => snd_bc1,
        A9n       => '0',
        A8        => '1',
        DA_IN     => std_ulogic_vector(gdphs_wbs_writedata(7 downto 0)),
        DA_OUT    => snd_data,
        DA_EN     => open,
        IO_A_IN   => X"00",
        IO_A_OUT  => open,
        IO_A_EN   => open,
        IO_B_IN   => X"00",
        IO_B_OUT  => open,
        IO_B_EN   => open,
  --      OUT_A     => open,
  --      OUT_B     => open,
  --      OUT_C     => open
        PWM_OUT    => PWM_OUT_o
      );
  end generate;
  no_sound: if not use_sound_c generate
    snd_data       <= (others =>'0');
    --snd_cs         <= '0';
    snd_bdir       <= '0';
    snd_bc1        <= '0';
    PWM_OUT_o      <= '0';
    wav_cnt        <= 0;
    wav_en         <= '0';
  end generate;

  impl_SPI: if use_spi_c generate 
    --spi_cs <= IORQ when Addr(7 downto 1)=SPI_BASE_ADDR_c(7 downto 1) else -- 0x00 - 0x01
              --'0';
    
    SPI : SPI_Interface
      port map (
        reset_n_i   => reset_n,
        clk_i       => gdphs_clk,
        SD_SCK_o    => SD_SCK_o,
        SD_nCS_o    => SD_nCS_o,
        SD_MOSI_o   => SD_MOSI_o,
        SD_MISO_i   => SD_MISO_i,
        Adr_i       => std_ulogic_vector(gdphs_wbs_address(0 downto 0)),
        en_i        => spi_cs,
        DataIn_i    => std_ulogic_vector(gdphs_wbs_writedata(7 downto 0)),
        Rd_i        => gdp_Rd_clk,
        Wr_i        => gdphs_wbs_write,
        DataOut_o   => spi_data
      );
  end generate;
  no_spi: if not use_spi_c generate
    spi_data       <= (others =>'0');
    --spi_cs         <= '0';
    SD_SCK_o       <= '0';
    SD_nCS_o       <= (others => '1');
    SD_MOSI_o      <= SD_MISO_i;
  end generate;
  
  impl_T1: if use_timer_c generate 
    --t1_cs <= IORQ when Addr(7 downto 2)=T1_BASE_ADDR_c(7 downto 2) else -- 0x00 - 0x01
              --'0';
    
    T1 : Timer
      port map (
        reset_n_i   => reset_n,
        clk_i       => gdphs_clk,
        irq_o       => t1_irq,
        Adr_i       => std_ulogic_vector(gdphs_wbs_address(1 downto 0)),
        en_i        => t1_cs,
        DataIn_i    => std_ulogic_vector(gdphs_wbs_writedata(7 downto 0)),
        Rd_i        => gdp_Rd_clk,
        Wr_i        => gdphs_wbs_write,
        DataOut_o   => t1_data
      );
  end generate;
  no_T1: if not use_timer_c generate
    t1_data      <= (others =>'0');
    --t1_cs        <= '0';
    t1_irq       <= '0';
  end generate;
  
  
  --- INT ---
  gdphs_int <= '1' when t1_irq='1' else
            '0';	
end rtl;
