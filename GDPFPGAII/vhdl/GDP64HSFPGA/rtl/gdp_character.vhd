--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - Character rasterizer module
-- File        : gdp_character.vhd
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

entity gdp_character is
  generic(INT_CHR_ROM_g : boolean := true);
  port(clk_i        : in  std_ulogic;
       clk_en_i     : in  std_ulogic;
       reset_n_i    : in  std_ulogic;
       enable_i     : in  std_ulogic;
       ----------------------------------------------------------------------------------
       -- to command module (decoder)
       ----------------------------------------------------------------------------------
       -- start point
       x1_i          : in  coord_t;
       y1_i          : in  coord_t;
       
       --
       symbol_i      : in  symbol_t;
       color_i       : in  std_ulogic;
       cmd_stb_i     : in  std_ulogic;
       rom_page_i    : in  std_ulogic:='0';
       
       SymbolStyle_i : in  symbolStyle_t;
       scaleX_i      : in  scale_t;
       scaleY_i      : in  scale_t;
       raster_bg_i   : in  std_ulogic;
       
       -- busy strobe for decoder, indicates that module is working
       busy_o        : out std_ulogic;
       ------------------------------------------------------------------------
       -- interface to vram controller
       ------------------------------------------------------------------------
       -- coordinates of current pixel
       posx_o          : out coord_t;
       posy_o          : out coord_t;
       -- color write request
       wr_req_o        : out std_ulogic;
       -- data to write
       wr_pixel_o      : out std_ulogic;
       -- acknowledge that the data to write has been taken
       wr_ack_i        : in  std_ulogic;
       -- force write
--       last_wr_o       : out std_ulogic;
       -- indicates that last command hasn't been completed yet
--       busy_wr_i       : in  std_ulogic;
       ------------------------------------------------------------------------
       -- interface to char ROM (used only when INT_CHR_ROM_g = false)
       ------------------------------------------------------------------------
       rom_addr_o      : out std_ulogic_vector(8 downto 0);
       rom_data_i      : in  std_ulogic_vector(7 downto 0);
       rom_ena_o       : out std_ulogic;
       rom_busy_i      : in  std_ulogic;
       --
       char_rom_addr_i : in  std_ulogic_vector(8 downto 0):="000000000";
       char_rom_data_i : in  std_ulogic_vector(7 downto 0):="00000000";
       char_rom_we_i   : in  std_ulogic:='0';
       --------------------------
       -- Monitoring (Debug) signals
       --------------------------
       monitoring_o: out std_ulogic_vector(7 downto 0)
       );

end gdp_character;

architecture RTL of gdp_character is

  component gdp_font
  	port(
  		Clk	     : in  std_ulogic;
  		clk_en   : in  std_ulogic;
  		A	       : in  std_ulogic_vector(8 downto 0);
  		D	       : out std_ulogic_vector(7 downto 0)
  	);
  end component;
  
  component gdp_font_ram is
  	port (
  		Clk			: in  std_ulogic;
  		En			: in  std_ulogic;
  		Wr			: in  std_ulogic;
  		DIn			: in  std_ulogic_vector(7 downto 0);
  		Addr	  : in  std_ulogic_vector(9 downto 0);
  		Dout		: out std_ulogic_vector(7 downto 0)
  	);
  end component;
    
   -- state definition
  type char_state_t is (idle_e, wait_rom_e, draw_char_e, finish_e);

  -- state signals
  signal state, next_state           : char_state_t;

  -- current x and y position
  signal posx, next_posx : coord_t;
  signal set_posx        : std_ulogic;
  signal posy, next_posy : coord_t;
  signal set_posy        : std_ulogic;
  -- x,y pixel counters
  signal xpcnt, next_xpcnt : unsigned(2 downto 0); 
  signal set_xpcnt         : std_ulogic;
  signal ypcnt, next_ypcnt : unsigned(3 downto 0);
  signal set_ypcnt         : std_ulogic;
  --  x,y scale counters
  signal xscnt, next_xscnt : unsigned(3 downto 0);
  signal set_xscnt         : std_ulogic;
  signal yscnt, next_yscnt : unsigned(3 downto 0);
  signal set_yscnt         : std_ulogic;

  signal first, next_first : std_ulogic;  
  -- delta signals, one bit larger than coords
--  signal dx, next_dx : std_ulogic_vector(posWidth_c downto 0);
--  signal dy, next_dy : std_ulogic_vector(posWidth_c downto 0);

  -- write request
  signal next_wr_req : std_ulogic;
  signal wr_req      : std_ulogic;
  
  signal finished, next_finished : std_ulogic;

  -- command acknowledge expected signal
--  signal ack_expected, next_ack_expected : std_ulogic;
  signal wr_busy      : std_ulogic;
  -- force write
--  signal next_last_wr : std_ulogic;
  signal rom_addr     : std_ulogic_vector(9 downto 0);
  signal rom_data     : std_ulogic_vector(7 downto 0);
--  signal rom_data_tmp : std_logic_vector(7 downto 0);
  signal rom_ena      : std_ulogic;
  signal next_rom_ena : std_ulogic;
  signal wr_pixel     : std_ulogic;
  signal next_wr_pixel: std_ulogic;
 
begin

    use_int_rom : if INT_CHR_ROM_g generate
--      char_rom: gdp_font
--        port map (
--          Clk    => clk_i,
--          clk_en => clk_en_i,
--          A	     => rom_addr,
--          D	     => rom_data
--      );
  char_rom: gdp_font_ram
    port map (
  		Clk	   => clk_i,
  		En	   => clk_en_i,
  		Wr	   => char_rom_we_i,
  		DIn	   => char_rom_data_i,
  		Addr   => rom_addr,
  		Dout   => rom_data
    );

    	
--      rom_data   <= std_ulogic_vector(rom_data_tmp);
      rom_addr_o <= (others => '0');
      rom_ena_o  <= '0';
    end generate;
    
    use_ext_rom : if not INT_CHR_ROM_g generate
      rom_addr_o <= rom_addr;
      rom_ena_o  <= rom_ena;
      rom_data   <= rom_data_i;
    end generate;

    process(symbol_i,xpcnt,rom_page_i,char_rom_addr_i,char_rom_we_i)
      variable tmp_v : unsigned(rom_addr'high-1 downto 0);
    begin
      tmp_v    := resize(unsigned(symbol_i)-32,tmp_v'length);
      tmp_v    := shift_left(tmp_v,2) + tmp_v; -- *5
      rom_addr <= rom_page_i & std_ulogic_vector(tmp_v+unsigned(xpcnt));
      if char_rom_we_i='1' then
        rom_addr <= '1' & char_rom_addr_i;
      end if;
    end process;

    -----------------------------------------------------------------------------
    -- FSM registers
    -----------------------------------------------------------------------------
    fsm_regs: process (clk_i, reset_n_i)
    begin
      if reset_n_i = ResetActive_c then
        state            <= idle_e;
--        dx               <= (others => '0');
--        dy               <= (others => '0');
        posx             <= (others => '0');
        posy             <= (others => '0');
        first            <= '0';
        wr_req           <= inactivated_c;
--        ack_expected     <= inactivated_c;
--        last_wr_o        <= inactivated_c;
        xpcnt            <= (others => '0');
        ypcnt            <= (others => '0');
        xscnt            <= (others => '0');
        yscnt            <= (others => '0');
        wr_busy          <= inactivated_c;
        rom_ena          <= inactivated_c;
        wr_pixel         <= '0';
      elsif rising_edge(clk_i) then
        if clk_en_i = '1' then
          state            <= next_state;
  --        dx               <= next_dx;
  --        dy               <= next_dy;
          wr_pixel   <= next_wr_pixel;
          if set_posx = activated_c then
            posx           <= next_posx;
          end if;
          if set_posy = activated_c then
            posy           <= next_posy;
          end if;
          first            <= next_first;
          if set_xpcnt = activated_c then
            xpcnt          <= next_xpcnt;
          end if;
          if set_ypcnt = activated_c then
            ypcnt          <= next_ypcnt;
          end if;
          if set_xscnt = activated_c then
            xscnt          <= next_xscnt;
          end if;
          if set_yscnt = activated_c then
            yscnt          <= next_yscnt;
          end if;
          if not INT_CHR_ROM_g then
            rom_ena        <= next_rom_ena;
          end if;
         
          wr_req           <= next_wr_req;
  --        ack_expected     <= next_ack_expected;
          finished         <= next_finished;
--          last_wr_o        <= next_last_wr;
          if next_wr_req = activated_c then
            wr_busy <= activated_c;
          
          elsif wr_ack_i = activated_c then
            wr_busy <= inactivated_c;
          end if;
        end if;
        
      end if;
    end process fsm_regs;
    
--    color_wr_req_o   <= wr_req;
    wr_req_o   <= wr_req;
    
    
    -----------------------------------------------------------------------------
    -- Determine next state and output
    -----------------------------------------------------------------------------
    fsm_comb: process (wr_busy, first, SymbolStyle_i, raster_bg_i, color_i,
                       posx, posy, state, cmd_stb_i, scaleX_i, scaleY_i,
                       xpcnt, next_xpcnt, ypcnt, next_ypcnt,
                       xscnt, next_xscnt, yscnt, next_yscnt, rom_data,
                       wr_ack_i, x1_i,y1_i, finished, wr_pixel,
                       enable_i,wr_req, rom_busy_i, rom_ena)

      function calc_delta_f(size : in scale_t) return natural is
        variable size_v  : natural range 1 to 16;
        variable delta_v : std_ulogic_vector(7 downto 0);
      begin
        size_v := to_integer(unsigned(size)-1)+1;
--        delta_v := std_ulogic_vector(shift_left(to_unsigned(size_v,delta_v'length),2) +
--                                     shift_left(to_unsigned(size_v,delta_v'length),1));   -- *6
        delta_v := std_ulogic_vector(shift_left(to_unsigned(size_v,delta_v'length),3) - 2);
        return to_integer(unsigned(delta_v));
      end calc_delta_f;
    begin
    
      -- default assignments
      next_state           <= state;
      next_posx            <= (others => '-');
      set_posx             <= inactivated_c;
      next_posy            <= (others => '-');
      set_posy             <= inactivated_c;
--      next_dx              <= dx;
--      next_dy              <= dy;
      next_first           <= first;
      busy_o               <= activated_c;

      next_wr_req          <= inactivated_c;
--      next_ack_expected    <= ack_expected;
      next_finished        <= finished;
--      next_last_wr         <= inactivated_c;
      next_xpcnt           <= (others => '-');
      set_xpcnt            <= inactivated_c;
      next_ypcnt           <= (others => '-');
      set_ypcnt            <= inactivated_c;
      next_xscnt           <= (others => '-');
      set_xscnt            <= inactivated_c;
      next_yscnt           <= (others => '-');
      set_yscnt            <= inactivated_c;
      next_rom_ena         <= inactivated_c;
      next_wr_pixel        <= wr_pixel;
      
      case state is
        when idle_e =>
          busy_o            <= inactivated_c;
          next_finished     <= inactivated_c;
--          next_ack_expected <= inactivated_c;
    
          -- wait until inputs are flagged valid and
          -- last write access is completed
          if (enable_i and cmd_stb_i) = activated_c then 

            -- rasterizer line starting point is always (x1,y1)
            set_posx   <= activated_c;
            next_posx  <= x1_i;
            set_posy   <= activated_c;
            next_posy  <= y1_i;
            next_xpcnt <= (others => '0');
            set_xpcnt  <= activated_c;
            next_ypcnt <= (others => '0');
            set_ypcnt  <= activated_c;
            next_xscnt <= (others => '0');
            set_xscnt  <= activated_c;
            next_yscnt <= (others => '0');
            set_yscnt  <= activated_c;
--            next_first <= activated_c;
            -- proceed to setup state
            next_state <= wait_rom_e;
          end if;

        when wait_rom_e =>
--          if INT_CHR_ROM_g or 
--             (rom_ena or rom_busy_i)='0' then
            -- one idle cycle for Char-ROM
            next_state <= draw_char_e;
            next_first <= activated_c;
--          end if;
        -----------------------------------------------------------------------
        -- draw symbols
        -----------------------------------------------------------------------
        when draw_char_e =>
          next_first <= inactivated_c;
          -- wait until acknowledge of last request occurs
          if wr_busy = inactivated_c or wr_ack_i = activated_c then
            if ((raster_bg_i and color_i) or rom_data(to_integer(not ypcnt(2 downto 0))))='1' then
              next_wr_req   <= activated_c;
              next_wr_pixel <= color_i;
            end if;
            if (raster_bg_i and color_i)='1' then
              -- only activate bg-raster mode when active color is fg (Wr-Pen)
              next_wr_pixel <= rom_data(to_integer(not ypcnt(2 downto 0)));
            end if;
            if first = inactivated_c then
              if SymbolStyle_i(1)='0' then
                -- normal draw
                set_posy   <= activated_c;
                next_posy  <= std_ulogic_vector(unsigned(posy) + 1);
                if SymbolStyle_i(0)='1' then
                  -- tilted draw. increment x-coordinate also
                  set_posx   <= activated_c;
                  next_posx  <= std_ulogic_vector(unsigned(posx) + 1);
                end if;
              else
                -- rotated draw
                set_posx   <= activated_c;
                next_posx  <= std_ulogic_vector(unsigned(posx) - 1);
                if SymbolStyle_i(0)='1' then
                  -- tilted draw. increment x-coordinate also
                  set_posy   <= activated_c;
                  next_posy  <= std_ulogic_vector(unsigned(posy) + 1);
                end if;
              end if;
            end if;
            if ypcnt /= 8 and yscnt < (to_integer(unsigned(scaleY_i)-1)) then
              set_yscnt  <= activated_c;
              next_yscnt <= yscnt + 1;
            else
              set_yscnt  <= activated_c;
              next_yscnt <= (others => '0');
              if ypcnt /= 8 then
                set_ypcnt  <= activated_c;
                next_ypcnt <= ypcnt + 1;
              else
                set_ypcnt  <= activated_c;
                next_ypcnt <= (others => '0');
                if SymbolStyle_i(1)='0' then
                  -- normal draw
                  set_posy   <= activated_c;
                  next_posy  <= y1_i;
                  if SymbolStyle_i(0)='1' then
                    -- tilted draw.
                    set_posx   <= activated_c;
                    next_posx  <= std_ulogic_vector(unsigned(posx) - calc_delta_f(scaleY_i));  -- 8*yscale - 2
                  else
                    set_posx   <= activated_c;
                    next_posx  <= std_ulogic_vector(unsigned(posx) + 1);
                  end if;
                  
                else
                  -- rotated draw
                  set_posx   <= activated_c;
                  next_posx  <= x1_i;
                  if SymbolStyle_i(0)='1' then
                    -- tilted draw.
                    set_posy   <= activated_c;
                    next_posy  <= std_ulogic_vector(unsigned(posy) - calc_delta_f(scaleY_i)); -- 8*yscale - 2
                  else
                    set_posy   <= activated_c;
                    next_posy  <= std_ulogic_vector(unsigned(posy) + 1);
                  end if;
                end if;
                next_state   <= wait_rom_e;
                next_rom_ena <= activated_c;
                next_wr_req <= inactivated_c;
                if xscnt < (to_integer(unsigned(scaleX_i)-1)) then
                  set_xscnt  <= activated_c;
                  next_xscnt <= xscnt + 1;
                else
                  set_xscnt  <= activated_c;
                  next_xscnt <= (others => '0');
                  set_xpcnt  <= activated_c;
                  next_xpcnt <= xpcnt + 1;
                  if xpcnt = 4 then
                    next_state   <= finish_e;
                  end if;
                end if;
              end if;
            end if;
          end if;
          

        when finish_e =>
          if wr_req='0' and
             (wr_busy = inactivated_c or wr_ack_i = activated_c) then
            next_state <= idle_e;
            busy_o     <= inactivated_c;
          end if; 
        when others =>
          next_state <= idle_e;
      end case;

      if enable_i = inactivated_c then
        next_state <= idle_e;
      end if;
    
    end process fsm_comb;
   
    -- build pixel
--    wr_data_o(rgb_color_t'range)                                         <= color_i;
--    wr_data_o(alpha_t'high+rgb_color_t'high+1 downto rgb_color_t'high+1) <= alpha_i;
--    wr_pixel_o <= color_i;
    wr_pixel_o <= wr_pixel;
    
    posx_o    <= posx;
    posy_o    <= posy;

--  with state select
--    monitoring_o(1 downto 0) <=  "00" when idle_e,
--                                 "01" when wait_rom_e,
--                                 "10" when draw_char_e,
--                                 "11" when finish_e;
--    monitoring_o(2) <= first;
--    monitoring_o(4) <= wr_busy;
--    monitoring_o(5) <= SymbolStyle_i(0);
--    monitoring_o(6) <= SymbolStyle_i(1);
--    monitoring_o(7) <= '0';
  monitoring_o <= symbol_i;
end rtl;
