--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - kernel
-- File        : GDP_kernel.vhd
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

-- CLUT: 0xA4    = Addressregister
--       0xA5,A6 = Datenregister

entity gdp_top is
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
--       pixel_o    : out std_ulogic;
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
	   	SRAM_ADDR   : out std_logic_vector(17 downto 0);	
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
end gdp_top;


architecture rtl of gdp_top is

  component gdp_video
    port(
      reset_n_i     : in  std_ulogic;
      clk_i         : in  std_ulogic;
      clk_en_i      : in  std_ulogic;
      rd_req_o      : out std_ulogic;
      rd_addr_o     : out std_ulogic_vector(15 downto 0);
      rd_data_i     : in  std_ulogic_vector(7 downto 0);
      rd_ack_i      : in  std_ulogic;
      rd_busy_i     : in  std_ulogic;
      scroll_i      : in  std_ulogic_vector(6 downto 0);
      enable_i      : in  std_ulogic;
      clut_we_i     : in  std_ulogic;
      clut_addr_i   : in  std_ulogic_vector(3 downto 0);
      clut_data_i   : in  std_ulogic_vector(8 downto 0);
      pixel_red_o   : out std_ulogic_vector(2 downto 0);
      pixel_green_o : out std_ulogic_vector(2 downto 0);
      pixel_blue_o  : out std_ulogic_vector(2 downto 0);
      Hsync_o       : out std_ulogic;
      Vsync_o       : out std_ulogic;
	  blank_o		: out std_logic;
	  ------------------------------------------------------------------------
      -- Hardware-Cursor (to VIDEO section)
      ------------------------------------------------------------------------
	  hwcuren_i		: in std_ulogic; -- hardware cursor enable (CTRL2.6)
	  curcol_i		: in std_ulogic_vector(3 downto 0);	 -- current FG color
	  cx1_i			: in std_ulogic_vector(11 downto 0);
	  cx2_i			: in std_ulogic_vector(11 downto 0);
	  cy1_i			: in std_ulogic_vector(11 downto 0);
	  cy2_i			: in std_ulogic_vector(11 downto 0);
	  --------------------------
      -- Monitoring (Debug) signals
      --------------------------
      monitoring_o: out std_ulogic_vector(7 downto 0)
    );
  end component;

  component gdp_kernel
    generic(INT_CHR_ROM_g : boolean := true); 
    port (
      reset_n_i       : in  std_ulogic;
      clk_i           : in  std_ulogic;
      clk_en_i        : in  std_ulogic;
      Adr_i           : in  std_ulogic_vector(3 downto 0);
      CS_i            : in  std_ulogic;
      DataIn_i        : in  std_ulogic_vector(7 downto 0);
      Rd_i            : in  std_ulogic;
      Wr_i            : in  std_ulogic;
      DataOut_o       : out std_ulogic_vector(7 downto 0);
      rmw_mode_i      : in  std_ulogic;
      vsync_i         : in  std_ulogic;
      hsync_i         : in  std_ulogic;
      vidEnable_o     : out std_ulogic;
      DMAData_o       : out std_ulogic_vector(7 downto 0);
      color_reg_i     : in  std_ulogic_vector(7 downto 0);
      kernel_req_o    : out std_ulogic;
      kernel_wr_o     : out std_ulogic;
      kernel_addr_o   : out std_ulogic_vector(15 downto 0);
      kernel_data_o   : out std_ulogic_vector(7 downto 0);
      kernel_data_i   : in  std_ulogic_vector(7 downto 0);
      kernel_busy_i   : in  std_ulogic;
      kernel_ack_i    : in  std_ulogic;
      chr_rom_addr_o  : out std_ulogic_vector(8 downto 0);
      chr_rom_data_i  : in  std_ulogic_vector(7 downto 0);
      chr_rom_ena_o   : out std_ulogic;
      chr_rom_busy_i  : in  std_ulogic;
	  ------------------------------------------------------------------------
      -- Hardware-Cursor (to VIDEO section)
      ------------------------------------------------------------------------
	   hwcuren_o		: out std_ulogic; -- hardware cursor enable ( CTRL2.6 )	   
	   cx1_o			: out std_ulogic_vector(11 downto 0);
	   cx2_o			: out std_ulogic_vector(11 downto 0);
	   cy1_o			: out std_ulogic_vector(11 downto 0);
	   cy2_o			: out std_ulogic_vector(11 downto 0);
	   
      monitoring_o    : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
    );
  end component;

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
	  
      SRAM_nCS0    : out std_logic;
      SRAM_nCS1   : out std_logic;       
	  SRAM_ADDR   : out std_logic_vector(17 downto 0);	
      SRAM_DB     : inout std_logic_vector(15 downto 0);
      SRAM_nWR    : out std_logic;
      SRAM_nOE    : out std_logic;
	  SRAM_nBHE	: out std_logic;						
	  SRAM_nBLE	: out std_logic;
	   	
      -- ext. ROM signals
      rom_ena_o         : out std_ulogic;
	  --------------------------
      -- Monitoring (Debug) signals
      --------------------------
      monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
    );
  end component;

  signal DataOut_s		 : std_ulogic_vector(15 downto 0);
  signal monitoring      : std_ulogic_vector(nr_mon_sigs_c-1 downto 0);
  signal kernel_req      : std_ulogic;
  signal kernel_wr       : std_ulogic;
  signal kernel_addr     : std_ulogic_vector(17 downto 0);
  signal kernel_addr1    : std_ulogic_vector(15 downto 0);
  signal kernel_rd_data  : std_ulogic_vector(7 downto 0);
  signal kernel_wr_data  : std_ulogic_vector(7 downto 0);
  signal kernel_busy     : std_ulogic;
  signal kernel_ack      : std_ulogic;

  signal host_req        : std_ulogic;
  signal host_wr         : std_ulogic;
  signal host_addr       : std_ulogic_vector(18 downto 0);
  signal host_rd_data    : std_ulogic_vector(15 downto 0);
  signal host_wr_data    : std_ulogic_vector(15 downto 0);
  signal host_sel		 : std_ulogic_vector(1 downto 0);
  signal host_busy       : std_ulogic;
  signal host_ack        : std_ulogic;
	
  signal vid_rd_req      : std_ulogic;
  signal vid_rd_addr     : std_ulogic_vector(17 downto 0);
  signal vid_rd_addr1    : std_ulogic_vector(15 downto 0);
  signal vid_rd_data     : std_ulogic_vector(7 downto 0);
  signal vid_rd_busy     : std_ulogic;
  signal vid_rd_ack      : std_ulogic;
--  signal gdp_en,sfr_en   : std_ulogic;
  signal page_reg        : std_ulogic_vector(4 downto 0);
  signal scroll_reg      : std_ulogic_vector(6 downto 0);
  signal vid_enable      : std_ulogic;
  signal dma_data        : std_ulogic_vector(7 downto 0);
  signal kernel_DataOut  : std_ulogic_vector(7 downto 0);
  signal vsync,hsync     : std_ulogic;
  signal chr_rom_addr    : std_ulogic_vector(8 downto 0);
  signal chr_rom_data    : std_ulogic_vector(7 downto 0);
  signal chr_rom_ena     : std_ulogic;
  signal chr_rom_busy    : std_ulogic;
  signal color_reg       : std_ulogic_vector(7 downto 0):= X"00";
  signal clut_addr       : std_ulogic_vector(3 downto 0):= X"0";
  signal temp_reg        : std_ulogic:= '0';
  signal clut_we         : std_ulogic;
  signal clut_data       : std_ulogic_vector(8 downto 0);
  signal pixel_red       : std_ulogic_vector(2 downto 0);
  signal pixel_green     : std_ulogic_vector(2 downto 0);
  signal pixel_blue      : std_ulogic_vector(2 downto 0);
  
  ------------------------------------------------------------------------
  -- Hardware-Cursor (to VIDEO section)
  ------------------------------------------------------------------------
  signal hwcuren		: std_ulogic; -- hardware cursor enable ( CTRL1
  signal cx1			: std_ulogic_vector(11 downto 0);
  signal cx2			: std_ulogic_vector(11 downto 0);
  signal cy1			: std_ulogic_vector(11 downto 0);
  signal cy2			: std_ulogic_vector(11 downto 0);
  signal blank			: std_ulogic;
begin

 video : gdp_video
  port map (
    reset_n_i  => reset_n_i,
    clk_i      => clk_i,
    clk_en_i   => clk_en_i,
    -----------------------------
    -- interface to VRAM
    -----------------------------
    rd_req_o   => vid_rd_req,
    rd_addr_o  => vid_rd_addr1, --(13 downto 0),
    rd_data_i  => vid_rd_data,
    rd_ack_i   => vid_rd_ack ,
    rd_busy_i  => vid_rd_busy,
    -----------------------------
    scroll_i   => scroll_reg,
    enable_i   => vid_enable,
    -----------------------------
    clut_we_i     => clut_we,
    clut_addr_i   => clut_addr,
    clut_data_i   => clut_data,
    -----------------------------
--    pixel_o    => pixel_o,
    pixel_red_o   => pixel_red,  
    pixel_green_o => pixel_green,
    pixel_blue_o  => pixel_blue, 
    Hsync_o       => Hsync,
    Vsync_o       => Vsync,
	
	------------------------------------------------------------------------
    -- Hardware-Cursor (to VIDEO section)
    ------------------------------------------------------------------------
	hwcuren_i		=> hwcuren, -- hardware cursor enable ( CTRL1
	curcol_i		=> color_reg(3 downto 0),
	cx1_i			=> cx1,
	cx2_i			=> cx2,
	cy1_i			=> cy1,
	cy2_i			=> cy2,
	------------------------------
	blank_o		    => blank,
	------------------------------
	monitoring_o    => open
  );
  
  Hsync_o       <= Hsync;
  Vsync_o       <= Vsync;
  pixel_red_o   <= pixel_red;  
  pixel_green_o <= pixel_green;
  pixel_blue_o  <= pixel_blue;

--  vid_rd_addr(15 downto 14) <= page_reg(2 downto 1); -- read bank
  vid_rd_addr <= page_reg(2 downto 1) & vid_rd_addr1 when color_support_c else
                 "00"&page_reg(2 downto 1) & vid_rd_addr1(13 downto 0);

  kernel: gdp_kernel
    generic map(INT_CHR_ROM_g => INT_CHR_ROM_g)
    port map (
      reset_n_i      => reset_n_i,
      clk_i          => clk_i,
      clk_en_i       => clk_en_i,
      Adr_i          => Adr_i(3 downto 0),
      CS_i           => gdp_en_i,
      DataIn_i       => DataIn_i(7 downto 0),
      Rd_i           => Rd_i,
      Wr_i           => Wr_i,
      DataOut_o      => kernel_DataOut,
      rmw_mode_i     => page_reg(0),
      vsync_i        => Vsync,
      hsync_i        => Hsync,
      vidEnable_o    => vid_enable,
      DMAData_o      => dma_data,
      color_reg_i    => color_reg, --X"65", -- BG: 7..4, FG: 3..0
      kernel_req_o   => kernel_req, 
      kernel_wr_o    => kernel_wr,  
      kernel_addr_o  => kernel_addr1,
      kernel_data_o  => kernel_wr_data,
      kernel_data_i  => kernel_rd_data,
      kernel_busy_i  => kernel_busy,
      kernel_ack_i   => kernel_ack,
      chr_rom_addr_o => chr_rom_addr,
      chr_rom_data_i => chr_rom_data,
      chr_rom_ena_o  => chr_rom_ena,
      chr_rom_busy_i => chr_rom_busy,
	  ------------------------------------------------------------------------
       -- Hardware-Cursor (to VIDEO section)
       ------------------------------------------------------------------------
	   hwcuren_o	=> hwcuren, -- hardware cursor enable ( CTRL1
	   cx1_o		=> cx1,
	   cx2_o		=> cx2,
	   cy1_o		=> cy1,
	   cy2_o		=> cy2,
      monitoring_o   => open
    );
  

--  kernel_addr(15 downto 14) <= page_reg(4 downto 3); -- write bank
  kernel_addr <= page_reg(4 downto 3) & kernel_addr1 when color_support_c else
                 "00"&page_reg(4 downto 3) & kernel_addr1(13 downto 0);
  
  vram : gdp_vram
    generic map(INT_CHR_ROM_g => INT_CHR_ROM_g)
    port map(
      clk_i           => clk_i,
      clk_en_i        => clk_en_i,
      reset_n_i       => reset_n_i,
	  blank_i		=> blank,
	  
	  -- host port (read & write 16bit)
	  host_req_i        => host_req,
      host_wr_i         => host_wr,
      host_addr_i       => host_addr,
      host_data_o    => host_rd_data,
      host_data_i    => host_wr_data,
      host_sel_i		=> host_sel,
      host_busy_o       => host_busy,
      host_ack_o        => host_ack,
	  
      kernel_clk_en_i => '1',
      kernel_req_i    => kernel_req,
      kernel_wr_i     => kernel_wr,
      kernel_addr_i   => kernel_addr,
      kernel_data_i   => kernel_wr_data,
      kernel_data_o   => kernel_rd_data,
      kernel_busy_o   => kernel_busy,
      kernel_ack_o    => kernel_ack,
      chr_rom_addr_i  => chr_rom_addr,
      chr_rom_data_o  => chr_rom_data,
      chr_rom_ena_i   => chr_rom_ena, 
      chr_rom_busy_o  => chr_rom_busy,
      rd_req_i        => vid_rd_req,
      rd_addr_i       => vid_rd_addr,
      rd_data_o       => vid_rd_data,
      rd_busy_o       => vid_rd_busy,
      rd_ack_o        => vid_rd_ack,
      
      SRAM_nCS0   =>    SRAM_nCS0  ,
	  SRAM_nCS1   =>    SRAM_nCS1  ,
	  SRAM_ADDR   =>    SRAM_ADDR  ,
	  SRAM_DB     =>    SRAM_DB    ,
	  SRAM_nWR    =>    SRAM_nWR   ,
	  SRAM_nOE    =>    SRAM_nOE   ,
	  SRAM_nBHE   => 	SRAM_nBHE	,
	  SRAM_nBLE   =>    SRAM_nBLE  ,

	  -- debug out
	  monitoring_o			  => monitoring_o
    );


  
  -- Prozess zum schreiben der SFR's
  Regs : process(clk_i, reset_n_i)
  begin
    if reset_n_i = ResetActive_c then
      page_reg   <= (others => '0');
      scroll_reg <= (others => '0');
      if color_support_c then
        color_reg <= X"01"; -- bg: black, fg: white
        if use_clut_c then
          clut_addr <= (others => '0');
          temp_reg  <= '0';
        end if;
      end if;
    elsif rising_edge(clk_i) then
      if clk_en_i = '1' then
        if (sfr_en_i and Wr_i) = '1' then
          case to_integer(unsigned(Adr_i(3 downto 0))) is
            when 0  =>
              -- 0x60: page selection sfr
              page_reg(4 downto 1) <= DataIn_i(7 downto 4); -- read / write bank
              page_reg(0)          <= DataIn_i(0);          -- RMW-mode
            when 1  =>
              -- 0x61: Hardscroll
              scroll_reg           <= DataIn_i(7 downto 1);
            when others => null;
          end case;
        end if;
        if color_support_c and 
          (col_en_i and Wr_i) = '1' then
          case to_integer(unsigned(Adr_i(0 downto 0))) is
            when 0  =>
              -- 0xA0: FG Color
              color_reg(3 downto 0) <= DataIn_i(3 downto 0);
              if DataIn_i(3 downto 0)=X"0" and color_reg(7 downto 4)=X"0" then
                -- don't allow black on black !
                color_reg(3 downto 0) <= X"1";
              end if;
            when 1  =>
              -- 0xA1: BG Color
              color_reg(7 downto 4) <= DataIn_i(3 downto 0);
              if DataIn_i(3 downto 0)=X"0" and color_reg(3 downto 0)=X"0" then
                -- don't allow black on black !
                color_reg(3 downto 0) <= X"1";
              end if;
            when others => null;
          end case;
        elsif color_support_c and use_clut_c and
          (clut_en_i and Wr_i) = '1' then
          case to_integer(unsigned(Adr_i(1 downto 0))) is
            when 0  =>
              -- 0xA4: Address Register
              clut_addr <= DataIn_i(3 downto 0);
            when 1  =>
              -- 0xA5: Data high
              temp_reg <= DataIn_i(0);
--            when 2  =>
--              -- 0xA6: Data low
--              temp_reg <= DataIn_i(0);
            when others => null;
          end case;
        end if;
        if color_support_c and use_clut_c and clut_we='1' then
          clut_addr <= std_ulogic_vector(unsigned(clut_addr) + 1);
        end if;
      end if;
    end if;
  end process;

  clut_we <= (clut_en_i and Wr_i)  when color_support_c and use_clut_c and Adr_i(1 downto 0)="10" else
             '0';
  clut_data <= temp_reg & DataIn_i(7 downto 0) when color_support_c and use_clut_c else
               (others => '0');
   
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if clk_en_i = '1' then
        if (gdp_en_i and Rd_i) = '1' then
          DataOut_s(7 downto 0) <= kernel_DataOut;
        elsif (sfr_en_i and Rd_i) = '1' then
          DataOut_s(7 downto 0) <= dma_data;	
        elsif color_support_c and 
             (col_en_i and Rd_i) = '1' then
--          DataOut_s(7 downto 0) <= color_reg;
          case to_integer(unsigned(Adr_i(0 downto 0))) is
            when 0  =>
              -- 0xA0: FG Color
              DataOut_s(7 downto 0) <= "0000" & color_reg(3 downto 0);
            when 1  =>
              -- 0xA1: BG Color
              DataOut_s(7 downto 0) <= "0000" & color_reg(7 downto 4);
            when others => null;
          end case;
        elsif color_support_c and use_clut_c and 
             (clut_en_i and Rd_i) = '1' then
          if Adr_i(1 downto 0) = "00" then
            DataOut_s(7 downto 0) <= "0000" & clut_addr(3 downto 0);
          end if;
        end if;
      end if;	
    end if;
  end process;  
  
  DataOut_o <= host_rd_data when sram_cs_i = '1' else
				DataOut_s;
  
  
  -- Whishbone ACK   
  DataAck_o <=  DataCycle_i when gdp_en_i = '1' else 
				DataCycle_i when sfr_en_i = '1'  else
				DataCycle_i when col_en_i = '1' else
				DataCycle_i when clut_en_i = '1' else
				host_ack when sram_cs_i = '1' else -- host SRAM access												
				'0';
				
				
  -- host SRAM access signals
  host_req        <= sram_cs_i;
  host_wr         <= Wr_i;
  host_addr       <= Adr_i(18 downto 0);
  --signal host_rd_data    : std_ulogic_vector(15 downto 0);
  host_wr_data    <= DataIn_i;
  host_sel		 <= DataSel_i;
  --signal host_busy       : std_ulogic; -- not used ?
  --DataAck_o <= host_ack;

end rtl;
