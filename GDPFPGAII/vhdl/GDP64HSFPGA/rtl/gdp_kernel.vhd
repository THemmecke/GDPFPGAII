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

entity gdp_kernel is
  generic(INT_CHR_ROM_g : boolean := true); 
  port(reset_n_i     : in  std_ulogic;
       clk_i         : in  std_ulogic;
       clk_en_i      : in  std_ulogic;
       --------------------------
       -- internal data bus (Register)
       --------------------------
       Adr_i     : in  std_ulogic_vector(3 downto 0);
       CS_i      : in  std_ulogic;
       DataIn_i  : in  std_ulogic_vector(7 downto 0);
       Rd_i      : in  std_ulogic;
       Wr_i      : in  std_ulogic;
       DataOut_o : out std_ulogic_vector(7 downto 0);
       --------------------------
       -- SFR's
       --------------------------
       rmw_mode_i  : in  std_ulogic;
       vsync_i     : in  std_ulogic;
       hsync_i     : in  std_ulogic;
       vidEnable_o : out std_ulogic;
       DMAData_o   : out std_ulogic_vector(7 downto 0);
       color_reg_i : in  std_ulogic_vector(7 downto 0);
       --------------------------
       -- Interface to VRAM
       --------------------------
       kernel_req_o      : out std_ulogic;
       kernel_wr_o       : out std_ulogic;
       kernel_addr_o     : out std_ulogic_vector(15 downto 0);
       kernel_data_o     : out std_ulogic_vector(7 downto 0);
       kernel_data_i     : in  std_ulogic_vector(7 downto 0);
       kernel_busy_i     : in  std_ulogic;
       kernel_ack_i      : in  std_ulogic;
       ------------------------------------------------------------------------
       -- interface to char ROM (used only when INT_CHR_ROM_g = false)
       ------------------------------------------------------------------------
       chr_rom_addr_o    : out std_ulogic_vector(8 downto 0);
       chr_rom_data_i    : in  std_ulogic_vector(7 downto 0);
       chr_rom_ena_o     : out std_ulogic;
       chr_rom_busy_i    : in  std_ulogic;
	   ------------------------------------------------------------------------
       -- Hardware-Cursor (to VIDEO section)
       ------------------------------------------------------------------------
	   hwcuren_o		: out std_ulogic; -- hardware cursor enable ( CTRL2.6 )
	   cx1_o			: out std_ulogic_vector(11 downto 0);
	   cx2_o			: out std_ulogic_vector(11 downto 0);
	   cy1_o			: out std_ulogic_vector(11 downto 0);
	   cy2_o			: out std_ulogic_vector(11 downto 0);
       --------------------------
       -- Monitoring (Debug) signals
       --------------------------
       monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
       );
end gdp_kernel;


architecture rtl of gdp_kernel is
  component gdp_bresenham
    port (
      clk_i       : in  std_ulogic;
      clk_en_i    : in  std_ulogic;
      reset_n_i   : in  std_ulogic;
      enable_i    : in  std_ulogic;
      x1_i        : in  coord_t;
      y1_i        : in  coord_t;
      dx_i        : in  delta_t;
      dy_i        : in  delta_t;
      color_i     : in  std_ulogic;
      cmd_stb_i   : in  std_ulogic;
      linestyle_i : in  linestyle_t;
      busy_o      : out std_ulogic;
      posx_o      : out coord_t;
      posy_o      : out coord_t;
      wr_req_o    : out std_ulogic;
      wr_pixel_o  : out std_ulogic;
      wr_ack_i    : in  std_ulogic;
--      last_wr_o   : out std_ulogic;
--      busy_wr_i   : in  std_ulogic;
      monitoring_o: out std_ulogic_vector(7 downto 0)
    );
  end component;
  component gdp_character
    generic(INT_CHR_ROM_g : boolean := true); 
    port(
       clk_i           : in  std_ulogic;
       clk_en_i        : in  std_ulogic;
       reset_n_i       : in  std_ulogic;
       enable_i        : in  std_ulogic;
       x1_i            : in  coord_t;
       y1_i            : in  coord_t;
       symbol_i        : in  symbol_t;
       color_i         : in  std_ulogic;
       cmd_stb_i       : in  std_ulogic;
       rom_page_i      : in  std_ulogic:='0';
       SymbolStyle_i   : in  symbolStyle_t;
       scaleX_i        : in  scale_t;
       scaleY_i        : in  scale_t;
       raster_bg_i     : in  std_ulogic;
       busy_o          : out std_ulogic;
       posx_o          : out coord_t;
       posy_o          : out coord_t;
       wr_req_o        : out std_ulogic;
       wr_pixel_o      : out std_ulogic;
       wr_ack_i        : in  std_ulogic;
--       last_wr_o       : out std_ulogic;
--       busy_wr_i       : in  std_ulogic;
       rom_addr_o      : out std_ulogic_vector(8 downto 0);
       rom_data_i      : in  std_ulogic_vector(7 downto 0);
       rom_ena_o       : out std_ulogic;
       rom_busy_i      : in  std_ulogic;
       char_rom_addr_i : in  std_ulogic_vector(8 downto 0):="000000000";
       char_rom_data_i : in  std_ulogic_vector(7 downto 0):="00000000";
       char_rom_we_i   : in  std_ulogic:='0';
       monitoring_o    : out std_ulogic_vector(7 downto 0)
     );
  end component;
  component gdp_decoder
    port (
      reset_n_i       : in  std_ulogic;
      clk_i           : in  std_ulogic;
      clk_en_i        : in  std_ulogic;
      posStartX_o     : out coord_t;
      posStartY_o     : out coord_t;
      deltaX_o        : out delta_t;
      deltaY_o        : out delta_t;
      symbol_o        : out symbol_t;
      color_o         : out std_ulogic;
      lineStyle_o     : out lineStyle_t;
      SymbolStyle_o   : out symbolStyle_t;
      scaleX_o        : out scale_t;
      scaleY_o        : out scale_t;
      raster_bg_o     : out std_ulogic;
      drawCmd_o       : out drawCmd_t;
      drawCmd_stb_o   : out std_ulogic;
      drawBusy_i      : in  std_ulogic;
      char_rom_page_o : out std_ulogic;
      char_rom_we_o   : out std_ulogic;
      char_rom_addr_o : out std_ulogic_vector(8 downto 0);
      neverLeave_o    : out std_ulogic;
      vidEnable_o     : out std_ulogic;
      irqEn_o         : out std_ulogic_vector(2 downto 0);
      vsync_i         : in  std_ulogic;
      hsync_i         : in  std_ulogic;
	  hwcuren_o	 : out std_ulogic; -- hardware cursor enable (CTRL2.7)
      Adr_i           : in  std_ulogic_vector(3 downto 0);
      CS_i            : in  std_ulogic;
      DataIn_i        : in  std_ulogic_vector(7 downto 0);
      Rd_i            : in  std_ulogic;
      Wr_i            : in  std_ulogic;
      DataOut_o       : out std_ulogic_vector(7 downto 0)
    );
  end component;

  constant XRES_c : natural := 512;
  constant YRES_c : natural := 256;

  type state_t is(idle_e, vram_busy_e, modify_write_e,  clear_screen_e, dma_e);

  signal state,next_state      : state_t;

  signal posStartX             : coord_t;
  signal posStartY             : coord_t;
  signal deltaX                : delta_t;
  signal deltaY                : delta_t;
  signal symbol                : symbol_t;
  signal color                 : std_ulogic;
  signal lineStyle             : lineStyle_t;
  signal SymbolStyle           : symbolStyle_t;
  signal scaleX                : scale_t;
  signal scaleY                : scale_t;
  signal raster_bg             : std_ulogic;
  signal drawCmd               : drawCmd_t;
  signal drawCmd_stb           : std_ulogic;
  signal drawBusy              : std_ulogic;
  signal enable                : std_ulogic;
  signal bres_stb,bres_busy    : std_ulogic;
  signal bres_posx             : coord_t;
  signal bres_posy             : coord_t;
  signal bres_wr_req           : std_ulogic;
  signal bres_wr_pixel         : std_ulogic;
--  signal bres_last_wr          : std_ulogic;
  signal posx,posx1            : coord_t;
  signal posy,posy1            : coord_t;
  signal wr_ack, next_wr_ack   : std_ulogic;
--  signal busy_wr               : std_ulogic;
  signal char_stb              : std_ulogic;
  signal char_busy             : std_ulogic;
  signal char_posx             : coord_t;
  signal char_posy             : coord_t;
  signal char_wr_req           : std_ulogic;
  signal char_wr_pixel         : std_ulogic;
--  signal char_last_wr          : std_ulogic;
  signal wr_req,wr_pixel       : std_ulogic;
  signal kernel_req            : std_ulogic;
  signal kernel_wr             : std_ulogic;
  signal stored_kernel_wr      : std_ulogic;
  signal cached_kernel_addr    : std_ulogic_vector(15 downto 0);
  signal kernel_addr           : std_ulogic_vector(15 downto 0);
  signal kernel_wr_data        : std_ulogic_vector(7 downto 0);
  signal kernel_rd_data        : std_ulogic_vector(7 downto 0);
  signal vram_busy             : std_ulogic;
  signal clrscr_busy, next_clrscr_busy : std_ulogic;
  signal neverLeave            : std_ulogic;
  signal clip                  : std_ulogic;
  signal set_DMAData           : std_ulogic;
  signal DMAData, next_DMAData : std_ulogic_vector(7 downto 0);
  signal char_rom_we           : std_ulogic;
  signal char_rom_addr         : std_ulogic_vector(8 downto 0);
  signal char_rom_page         : std_ulogic;
  
  signal cdx : coord_t;
  signal cdy : coord_t;
  signal hwcuren : std_ulogic;
  signal q : std_ulogic_vector(31 downto 0);
begin

  enable <= '1';
  dec: gdp_decoder
    port map (
      reset_n_i       => reset_n_i,
      clk_i           => clk_i,
      clk_en_i        => clk_en_i,
      posStartX_o     => posStartX,
      posStartY_o     => posStartY,
      deltaX_o        => deltaX,
      deltaY_o        => deltaY,
      symbol_o        => symbol,
      color_o         => color,
      lineStyle_o     => lineStyle,
      SymbolStyle_o   => SymbolStyle,
      scaleX_o        => scaleX,
      scaleY_o        => scaleY,
      raster_bg_o     => raster_bg,
      drawCmd_o       => drawCmd,
      drawCmd_stb_o   => drawCmd_stb,
      drawBusy_i      => drawBusy,
      char_rom_page_o => char_rom_page,
      char_rom_we_o   => char_rom_we,
      char_rom_addr_o => char_rom_addr,
      neverLeave_o    => neverLeave, 
      vidEnable_o     => vidEnable_o,
      irqEn_o         => open,
      vsync_i         => vsync_i,
      hsync_i         => hsync_i,
	  hwcuren_o	 	  => hwcuren, -- hardware cursor enable (CTRL2.7)
      Adr_i           => Adr_i,
      CS_i            => CS_i,
      DataIn_i        => DataIn_i,
      Rd_i            => Rd_i,
      Wr_i            => Wr_i,
      DataOut_o       => DataOut_o);

  bres: gdp_bresenham
    port map (
      clk_i       => clk_i,
      clk_en_i    => clk_en_i,
      reset_n_i   => reset_n_i,
      enable_i    => enable,
      x1_i        => posStartX,
      y1_i        => posStartY,
      dx_i        => deltaX,
      dy_i        => deltaY,
      color_i     => color,
      cmd_stb_i   => bres_stb,
      linestyle_i => linestyle,
      busy_o      => bres_busy,
      posx_o      => bres_posx,
      posy_o      => bres_posy,
      wr_req_o    => bres_wr_req,
      wr_pixel_o  => bres_wr_pixel,
      wr_ack_i    => wr_ack,
--      last_wr_o   => bres_last_wr,
--      busy_wr_i   => busy_wr,
      monitoring_o=> open --monitoring_o(15 downto 8)
      );

  char : gdp_character
    generic map(INT_CHR_ROM_g => INT_CHR_ROM_g)
    port map (
      clk_i           => clk_i,
      clk_en_i        => clk_en_i,
      reset_n_i       => reset_n_i,
      enable_i        => enable,
      x1_i            => posStartX,
      y1_i            => posStartY,
      symbol_i        => symbol,
      color_i         => color,
      cmd_stb_i       => char_stb,
      rom_page_i      => char_rom_page,
      SymbolStyle_i   => SymbolStyle,
      scaleX_i        => scaleX,
      scaleY_i        => scaleY,
      raster_bg_i     => raster_bg,
      busy_o          => char_busy,
      posx_o          => char_posx,
      posy_o          => char_posy,
      wr_req_o        => char_wr_req,
      wr_pixel_o      => char_wr_pixel,
      wr_ack_i        => wr_ack,
--      last_wr_o       => char_last_wr,
--      busy_wr_i       => busy_wr,
      rom_addr_o      => chr_rom_addr_o,
      rom_data_i      => chr_rom_data_i,
      rom_ena_o       => chr_rom_ena_o, 
      rom_busy_i      => chr_rom_busy_i, 
      char_rom_addr_i => char_rom_addr,
      char_rom_data_i => DataIn_i,
      char_rom_we_i   => char_rom_we,
      monitoring_o    => open --monitoring_o(15 downto 8)
    );


--  busy_wr  <= '0';

  bres_stb <= drawCmd_stb when drawCmd = drawLine_e else
              '0';
  char_stb <= drawCmd_stb when drawCmd = drawSymbol_e else
              '0';
  drawBusy <= bres_busy   when drawCmd = drawLine_e else
              char_busy   when drawCmd = drawSymbol_e else
              clrscr_busy when drawCmd = clearScreen_e or drawCmd = DMA_e else
              '0';
  posx1    <= bres_posx when drawCmd = drawLine_e else
              char_posx when drawCmd = drawSymbol_e else
              posStartX;
              
  posy1    <= bres_posy when drawCmd = drawLine_e else
              char_posy when drawCmd = drawSymbol_e else
              posStartY;

  wr_req   <= bres_wr_req when drawCmd = drawLine_e else
              char_wr_req when drawCmd = drawSymbol_e else
              '0';

  wr_pixel <= bres_wr_pixel when drawCmd = drawLine_e else
              char_wr_pixel;

  ----------------------------------------------------------
  -- enable hardware cursor (ctrl2.6):
  
  process(clk_i)
   variable tmp : unsigned(q'range);
	begin    
    if (clk_i'event and clk_i = '1') then      
	   tmp := unsigned(q) + 1;
	   q <= std_ulogic_vector(tmp);
	  end if;	  
   end process;
  
  hwcuren_o <= hwcuren when q(24) = '1' else '0'; -- cursor blink, i have put that here (not in gdp_video), so it could be changed by register settings
  -- hwcuren_o <= hwcuren; -- no blink
  -- calculate hardware cursor area:

  process (posStartX,posStartY,cdx,cdy)
   variable tmpx1 : unsigned(posStartX'range);
   variable tmpx2 : unsigned(posStartX'range);
   variable tmpy1 : unsigned(posStartY'range);
   variable tmpy2 : unsigned(posStartY'range);
  begin
   tmpx1 := unsigned(posStartX) - 1;
   tmpx2 := tmpx1 + unsigned(cdx);
   tmpy1 := YRES_c-unsigned(posStartY) - unsigned(cdy);
   tmpy2 := YRES_c-unsigned(posStartY);
   cx1_o <= std_ulogic_vector(tmpx1);
   cx2_o <= std_ulogic_vector(tmpx2);
   cy1_o <= std_ulogic_vector(shift_left(tmpy1,1)); -- video ist 512(x) x 256(y), but gdp_video actually uses 512(y) pixel (2 pixel per pixel)
   cy2_o <= std_ulogic_vector(shift_left(tmpy2,1));
  end process;
  
  with ScaleX select
	  cdx <= "000000000101" when "0001",
			 "000000001010" when "0010",
			 "000000001111" when "0011",
			 "000000010100" when "0100",
			 "000000011001" when "0101",
			 "000000011110" when "0110",
			 "000000100011" when "0111",
			 "000000101000" when "1000",
			 "000000101101" when "1001",
			 "000000110010" when "1010",
			 "000000110111" when "1011",
			 "000000111100" when "1100",
			 "000001000001" when "1101",
			 "000001000110" when "1110",
			 "000001001011" when "1111",
			 "000001010000" when others;
			 
  with ScaleY select
	  cdy <= "000000001000" when "0001",
			 "000000010000" when "0010",
			 "000000011000" when "0011",
			 "000000100000" when "0100",
			 "000000101000" when "0101",
			 "000000110000" when "0110",
			 "000000111000" when "0111",
			 "000001000000" when "1000",
			 "000001001000" when "1001",
			 "000001010000" when "1010",
			 "000001011000" when "1011",
			 "000001100000" when "1100",
			 "000001101000" when "1101",
			 "000001110000" when "1110",
			 "000001111000" when "1111",
			 "000010000000" when others;
  ----------------------------------------------------------			 
  
  process(posx1,posy1,neverLeave)
    variable andx_v,andy_v : coord_t;
  begin
    andx_v := (others => '1');
    andy_v := (others => '1');
    if neverLeave = '1' then
      andx_v(posWidth_c-1 downto 9) := (others => '0'); -- 0 - 511
      andy_v(posWidth_c-1 downto 8) := (others => '0'); -- 0 - 255
    end if;
    posx <= posx1 and andx_v;
    posy <= posy1 and andy_v;
  end process;
  


  process(state, wr_req, wr_pixel, cached_kernel_addr, posx, posy, vram_busy, kernel_rd_data,
          drawCmd_stb, drawCmd, clrscr_busy, rmw_mode_i, stored_kernel_wr, color, color_reg_i
         )
    variable cache_hit_v : boolean;
    variable idx_v       : natural;
    variable and_mask_v  : std_ulogic_vector(3 downto 0);
    variable pixel_v     : std_ulogic_vector(3 downto 0);
    
    procedure calc_addr_p is
      variable tmp_v : unsigned(kernel_addr'range);
    begin
    if not color_support_c then
        -- addr = posy*(512/8)+posx/8
        tmp_v            := "00" & resize(unsigned(posy),tmp_v'length-2);
        tmp_v            := shift_left(tmp_v,9 - 3);
        tmp_v            := tmp_v + shift_right(unsigned(posx),3);
        kernel_addr      <= "00" & std_ulogic_vector(tmp_v(kernel_addr'high-2 downto 0));
      else
        -- addr = posy*(512/2)+posx/2
        tmp_v            := resize(unsigned(posy),tmp_v'length);
        tmp_v            := shift_left(tmp_v,9 - 1);
        tmp_v            := tmp_v + shift_right(unsigned(posx),1);
        kernel_addr      <= std_ulogic_vector(tmp_v);
      end if;
      cache_hit_v      := (std_ulogic_vector(tmp_v) = cached_kernel_addr);
    end procedure;
    
    procedure issue_wr_req_p is
    begin
      if unsigned(posx) < XRES_c and unsigned(posy) < YRES_c then
        if vram_busy = '0' then
          calc_addr_p;
    --      if not cache_hit_v then
              kernel_req <= '1';
              kernel_wr  <= '0';
    --      end if;
          next_state <= modify_write_e;
        else
          next_state <= vram_busy_e;
        end if;
      else
        clip        <= '1';
        next_wr_ack <= '1';
      end if;
    end procedure;
  begin
    cache_hit_v      := false;
    next_state       <= state;
    kernel_addr      <= cached_kernel_addr;
    kernel_req       <= '0';
    kernel_wr        <= stored_kernel_wr;
    next_wr_ack      <= '0';
    kernel_wr_data   <= kernel_rd_data;
    next_clrscr_busy <= clrscr_busy;
    clip             <= '0';
    set_DMAData      <= '0';
    next_DMAData     <= (others => '-');
-- pragma translate_off
    for i in kernel_rd_data'range loop
      if is_x(kernel_rd_data(i)) then
        kernel_wr_data(i) <= '0';
      end if;
    end loop;
-- pragma translate_on    
    
    case state is
      when idle_e =>
        if wr_req = '1' then
          issue_wr_req_p;
        elsif drawCmd_stb = '1' and drawCmd = clearScreen_e then
          next_state     <= clear_screen_e;
          kernel_addr    <= (others => '0');
          kernel_req     <= '1';
          kernel_wr      <= '1';
          kernel_wr_data <= (others => '0');
          next_clrscr_busy <= '1';
        elsif drawCmd_stb = '1' and drawCmd = DMA_e then
          next_state     <= dma_e;
          kernel_addr    <= (others => '0');
          kernel_req     <= '1';
          kernel_wr      <= '0';
          next_clrscr_busy <= '1';
          calc_addr_p;
        end if;
      when vram_busy_e =>
        if vram_busy = '0' then
          calc_addr_p;
          kernel_req <= '1';
          kernel_wr  <= '0';
          next_state <= modify_write_e;
        end if;
      when modify_write_e =>
        if vram_busy ='0' then
          kernel_req     <= '1';
          kernel_wr      <= '1';
          next_wr_ack    <= '1';
          -- when xor mode is active xor old color with new one
          if wr_pixel='1' then
            -- xor mode only when write pen is active
            if not color_support_c then
              kernel_wr_data(to_integer(unsigned(not posx(2 downto 0)))) <= wr_pixel xor
                (kernel_rd_data(to_integer(unsigned(not posx(2 downto 0)))) and rmw_mode_i);
            else
              idx_v      := to_integer(unsigned(not posx(0 downto 0)));
              and_mask_v := (others => rmw_mode_i);
              pixel_v    := kernel_rd_data(3 downto 0);
              if idx_v =1 then
                pixel_v  := kernel_rd_data(7 downto 4);
              end if;
              if idx_v=0 then
                kernel_wr_data(3 downto 0) <= color_reg_i(3 downto 0) xor (pixel_v and and_mask_v);
              else
                kernel_wr_data(7 downto 4) <= color_reg_i(3 downto 0) xor (pixel_v and and_mask_v);
              end if;
--              if idx_v=0 then 
--                kernel_wr_data(3 downto 0) <= color_reg_i(3 downto 0) xor 
--                 (kernel_rd_data(3 downto 0) and and_mask_v);
--              else
--                kernel_wr_data(7 downto 4) <= color_reg_i(3 downto 0) xor 
--                 (kernel_rd_data(7 downto 4) and and_mask_v);
--              end if;
            end if;
          else
            if not color_support_c then
              -- clear pen always without XOR-mode
              kernel_wr_data(to_integer(unsigned(not posx(2 downto 0)))) <= wr_pixel;
            else
              idx_v      := to_integer(unsigned(not posx(0 downto 0)));
              if idx_v=0 then 
                kernel_wr_data(3 downto 0) <= color_reg_i(7 downto 4);
              else
                kernel_wr_data(7 downto 4) <= color_reg_i(7 downto 4);
              end if;
            end if;
          end if;
          next_state  <= idle_e;
        end if;
--      when wait_finish_e =>
--        if vram_busy = '0' then
--          next_state  <= idle_e;
--          next_wr_ack <= '1';
--        end if;
      when clear_screen_e =>
        kernel_wr <= '1';
        if vram_busy = '0' then
          kernel_req  <= '1';
          if color_support_c then
            if color ='1' then
              -- fill with fg color
              kernel_wr_data <= color_reg_i(3 downto 0) & color_reg_i(3 downto 0);
            else
              -- fill with bg color
              kernel_wr_data <= color_reg_i(7 downto 4) & color_reg_i(7 downto 4);
            end if;
          else
            kernel_wr_data <= (others => color);
          end if;
--          kernel_wr_data <= cached_kernel_addr(7 downto 0);
          if (not color_support_c and unsigned(cached_kernel_addr(13 downto 0)) = (YRES_c * XRES_c/8)-1) or
             (    color_support_c and unsigned(cached_kernel_addr)              = (YRES_c * XRES_c/2)-1) then
            next_state       <= idle_e;
            next_clrscr_busy <= '0';
          else
            if not color_support_c then
              kernel_addr <= "00" & std_ulogic_vector(unsigned(cached_kernel_addr(13 downto 0)) +1);
            else
              kernel_addr <= std_ulogic_vector(unsigned(cached_kernel_addr) +1);
            end if;
          end if;
        end if;
      when dma_e =>
        if vram_busy ='0' then
          next_state       <= idle_e;
          next_clrscr_busy <= '0';
          set_DMAData      <= '1';
          next_DMAData     <= not kernel_rd_data;
        end if;
      when others =>
        next_state <= idle_e;
    end case;
  end process;

  process(clk_i,reset_n_i)
  begin
    if reset_n_i = ResetActive_c then
      state              <= idle_e;
      cached_kernel_addr <= (others => '1');
      wr_ack             <= '0';
      clrscr_busy        <= '0';
      stored_kernel_wr   <= '0';
      DMAData            <= (others => '1');
    elsif rising_edge(clk_i) then
      if clk_en_i = '1' then
        state              <= next_state;
        cached_kernel_addr <= kernel_addr;
        stored_kernel_wr   <= kernel_wr;
        wr_ack             <= next_wr_ack;
        clrscr_busy        <= next_clrscr_busy;
        if set_DMAData = '1' then
          DMAData <= next_DMAData;
        end if;
      end if;
    end if;
  end process;
  
--  wr_ack             <= next_wr_ack;
  
  kernel_req_o   <= kernel_req;    
  kernel_wr_o    <= kernel_wr;    
  kernel_addr_o  <= kernel_addr;
  kernel_data_o  <= kernel_wr_data;
  kernel_rd_data <= kernel_data_i;
  vram_busy      <= kernel_busy_i;
  
  DMAData_o      <= DMAData;
  
  monitoring_o(0) <= kernel_busy_i;
  monitoring_o(1) <= kernel_req;
  monitoring_o(2) <= drawCmd_stb; --bres_wr_req;
  monitoring_o(3) <= char_stb;    --bres_stb;
  monitoring_o(4) <= drawBusy;
  monitoring_o(5) <= wr_ack;
  monitoring_o(6) <= wr_req;
  monitoring_o(7) <= char_busy; --bres_busy; --clip;

  with state select
    monitoring_o(10 downto 8) <= "000" when idle_e,
                                 "001" when vram_busy_e,
                                 "010" when modify_write_e,
                                 "011" when clear_screen_e,
                                 "100" when dma_e,
                                 "111" when others;
  monitoring_o(11) <= clrscr_busy;
  monitoring_o(15 downto 12) <= "0000";

end rtl;
