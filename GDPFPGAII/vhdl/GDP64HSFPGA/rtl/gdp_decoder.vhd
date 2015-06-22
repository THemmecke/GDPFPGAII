--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - command decoder
-- File        : GDP_decoder.vhd
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

entity gdp_decoder is
  port(reset_n_i     : in  std_ulogic;
       clk_i         : in  std_ulogic;
       clk_en_i      : in  std_ulogic;
       
       posStartX_o   : out coord_t;
       posStartY_o   : out coord_t;
       deltaX_o      : out delta_t;
       deltaY_o      : out delta_t;
       symbol_o      : out symbol_t;

       color_o       : out std_ulogic;
       lineStyle_o   : out lineStyle_t;
       SymbolStyle_o : out symbolStyle_t;
       scaleX_o      : out scale_t;
       scaleY_o      : out scale_t;
       raster_bg_o   : out std_ulogic;
       drawCmd_o     : out drawCmd_t;
       drawCmd_stb_o : out std_ulogic;
       drawBusy_i    : in  std_ulogic;
       --
       char_rom_page_o : out std_ulogic;
       char_rom_we_o   : out std_ulogic;
       char_rom_addr_o : out std_ulogic_vector(8 downto 0);

       --
       neverLeave_o  : out std_ulogic;
       vidEnable_o   : out std_ulogic;
       irqEn_o       : out std_ulogic_vector(2 downto 0);
       vsync_i       : in  std_ulogic;
       hsync_i       : in  std_ulogic;
	   
	   hwcuren_o	 : out std_ulogic; -- hardware cursor enable (CTRL2.6)
       --------------------------
       -- internal data bus
       --------------------------
       Adr_i     : in  std_ulogic_vector(3 downto 0);
       CS_i      : in  std_ulogic;
       DataIn_i  : in  std_ulogic_vector(7 downto 0);
       Rd_i      : in  std_ulogic;
       Wr_i      : in  std_ulogic;
       DataOut_o : out std_ulogic_vector(7 downto 0)
       );
end gdp_decoder;


architecture rtl of gdp_decoder is
  -- Register
  signal Status_Reg   : std_ulogic_vector(7 downto 0);
  signal Cmd_Reg      : std_ulogic_vector(7 downto 0);
  signal Ctrl1_Reg    : std_ulogic_vector(6 downto 0);
  signal Ctrl2_Reg    : std_ulogic_vector(6 downto 0);
  signal CSize_Reg    : std_ulogic_vector(7 downto 0);
  signal DeltaX_Reg   : std_ulogic_vector(8 downto 0);
  signal DeltaY_Reg   : std_ulogic_vector(8 downto 0);
  signal XPos_Reg     : std_ulogic_vector(11 downto 0);
  signal YPos_Reg     : std_ulogic_vector(11 downto 0);

  -- signals
  signal next_DataOut_s : std_ulogic_vector(7 downto 0);
  signal CMD_stb        : std_ulogic;
--  signal cmd_clrs       : std_ulogic;
--  signal cmd_dma        : std_ulogic;
  signal drawCmd        : drawCmd_t;
  signal drawCmd_stb    : std_ulogic;
  signal deltaX,deltaY  : delta_t;
  signal drawBusy_d     : std_ulogic;
  signal draw_finished  : std_ulogic;
--  signal dma_finished   : std_ulogic;
  signal fillScreen     : std_ulogic;
begin
 -- Lesen der Register (kombinatorisch)

  with to_integer(unsigned(Adr_i(3 downto 0))) select
    next_DataOut_s <=  Status_Reg                       when 0,
                       "0"&Ctrl1_Reg                    when 1,
                       "0"&Ctrl2_Reg                    when 2,
                       CSize_Reg                        when 3,
                       "0000000"&DeltaX_Reg(8)          when 4, 
                       DeltaX_Reg(7 downto 0)           when 5,
                       "0000000"&DeltaY_Reg(8)          when 6,
                       DeltaY_Reg(7 downto 0)           when 7,
                       "0000"&XPos_Reg(11 downto 8)     when 8,
                       XPos_Reg(7 downto 0)             when 9,
                       "0000"&YPos_Reg(11 downto 8)     when 10,
                       YPos_Reg(7 downto 0)             when 11,
                       Status_Reg                       when 15,
                       (others => '0')                  when others;


--  process(clk_i)
--  begin
--    if rising_edge(clk_i) then
--      if (clk_en_i and CS_i and Rd_i) = '1' then
        DataOut_o <= next_DataOut_s;
--      end if;
--    end if;
--  end process;

  -- Prozess zum schreiben der Register
  Regs : process(clk_i, reset_n_i)
    procedure Reset_regs_p is
    begin
      Ctrl1_Reg  <= (others => '0');
      Ctrl2_Reg  <= (others => '0');
      CSize_Reg  <= X"11";
      DeltaX_Reg <= (others => '0');
      DeltaY_Reg <= (others => '0');
      XPos_Reg   <= (others => '0');
      YPos_Reg   <= (others => '0');
      fillScreen <= '0';
    end Reset_regs_p;
    variable delta_v : std_ulogic_vector(DeltaX_Reg'range);
    variable size_v  : natural range 1 to 16;

    function calc_delta_f(delta : in natural; sign: in std_ulogic) return delta_t is
      variable ret_v : delta_t;
    begin
--      ret_v := std_ulogic_vector(to_signed(delta+1,ret_v'length));
      ret_v := std_ulogic_vector(to_signed(delta,ret_v'length));
      if sign='1' then
        ret_v := std_ulogic_vector(0 - signed(ret_v));
      end if;
      return std_ulogic_vector(ret_v);
    end calc_delta_f;

    function calc_new_pos_f(oldpos : in unsigned; delta : in signed; bits : in natural;inside : in std_ulogic) return coord_t is
      variable ret_v : signed(posWidth_c downto 0);
    begin
      ret_v := signed("0"&oldpos) + to_integer(delta);
      if inside='1' then
        ret_v(posWidth_c downto bits) := (others =>'0');
      end if;
      return std_ulogic_vector(ret_v(posWidth_c-1 downto 0));
    end calc_new_pos_f;

    procedure draw_symb_p(four_by_four : boolean) is
    begin
      drawCmd     <= drawSymbol_e;
      drawCmd_stb <= '1';
      deltaX      <= (others => '0');
      deltaY      <= (others => '0');
      if Ctrl2_Reg(3)='0' then
        -- draw at X-Axis
        size_v := to_integer(unsigned(CSize_Reg(7 downto 4))-1)+1;
      else
        size_v := to_integer(unsigned(CSize_Reg(3 downto 0))-1)+1;
      end if;
      
      if not four_by_four then
        delta_v := std_ulogic_vector(shift_left(to_unsigned(size_v,delta_v'length),2) +
                                    shift_left(to_unsigned(size_v,delta_v'length),1));   -- *6
      else
        delta_v := std_ulogic_vector(shift_left(to_unsigned(size_v,delta_v'length),2)); -- *4
      end if;
      if Ctrl2_Reg(3)='0' then
        -- draw at X-Axis
        deltaX <= std_ulogic_vector(resize(unsigned(delta_v),deltaX'length));
      else
        -- draw at Y-Axis
        deltaY <= std_ulogic_vector(resize(unsigned(delta_v),deltaY'length));
      end if;
    end draw_symb_p;


  begin
    if reset_n_i = ResetActive_c then
      Reset_regs_p;
--      Cmd_Reg       <= "0000"&cmd_CLRS_CLRXY_c;
--      CMD_stb       <= '1';
      Cmd_Reg       <= (others => '0');
      CMD_stb       <= '0';
--      cmd_clrs      <= '0';
--      cmd_dma       <= '0';
      drawCmd       <= idle_e;
      drawCmd_stb   <= '0';
      deltaX        <= (others => '0');
      deltay        <= (others => '0');
      drawBusy_d    <= '0';

    elsif rising_edge(clk_i) then
      if clk_en_i ='1' then
        CMD_stb     <= '0';
        drawCmd_stb <= '0';
        drawBusy_d  <= drawBusy_i;
        if (CS_i and Wr_i) = '1' then
          case to_integer(unsigned(Adr_i(3 downto 0))) is
            when 0  =>
              -- only accept new commands if decoder is IDLE
              if Status_Reg(2) ='1' then
                Cmd_Reg                       <= DataIn_i;
                CMD_stb                       <= '1';
              end if;                         
            when 1  =>                        
              Ctrl1_Reg                       <= DataIn_i(Ctrl1_Reg'range);
            when 2  =>                        
              Ctrl2_Reg                       <= DataIn_i(Ctrl2_Reg'range);
            when 3  =>                        
              CSize_Reg                       <= DataIn_i;
            when 4  =>                        
              DeltaX_Reg(8)                   <= DataIn_i(0);
            when 5  =>                        
              DeltaX_Reg(7 downto 0)          <= DataIn_i;
            when 6  =>                        
              DeltaY_Reg(8)                   <= DataIn_i(0);
            when 7  =>                        
              DeltaY_Reg(7 downto 0)          <= DataIn_i;
            when 8  =>                        
              XPos_Reg(11 downto 8)           <= DataIn_i(3 downto 0);
            when 9  =>                        
              XPos_Reg(7 downto 0)            <= DataIn_i(7 downto 0);
            when 10 =>                        
              YPos_Reg(11 downto 8)           <= DataIn_i(3 downto 0);
            when 11 =>                        
              YPos_Reg(7 downto 0)            <= DataIn_i(7 downto 0);
            when 14 =>
              XPos_Reg(char_rom_addr_o'range) <= std_ulogic_vector(unsigned(XPos_Reg(char_rom_addr_o'range)) +1);
            when others  => null;
          end case;
        else
          ------------------------------------------------------
          -- Command decoder
          ------------------------------------------------------
          if CMD_stb='1' then
            fillScreen  <= '0';
            if Cmd_Reg(7 downto 4)=X"0" then
              case Cmd_Reg(3 downto 0) is
                when cmd_WPEN_c => -- 0
                  -- Enable Write-Pen (Set CTRL1.1 = '1')
                  Ctrl1_Reg(1) <= '1';
                when cmd_RPEN_c =>  -- 1
                  -- Enable Rubber-Pen (Set CTRL1.1 = '0')
                  Ctrl1_Reg(1) <= '0';
                when cmd_PEN_DOWN_c => -- 2
                  -- Pen down (Set CTRL1.0 = '1')
                  Ctrl1_Reg(0) <= '1';
                when cmd_PEN_UP_c =>  -- 3
                  -- Pen up (Set CTRL1.0 = '0')
                  Ctrl1_Reg(0) <= '0';
                when cmd_CLRS_c => -- 4
                  -- Clear Screen
  --                cmd_clrs <= '1';
                  drawCmd     <= clearScreen_e;
                  drawCmd_stb <= '1';
                when cmd_CLRXY_c => -- 5
                  -- Set X & Y Register to 0
                  XPos_Reg <= (others => '0');
                  YPos_Reg <= (others => '0');
                when cmd_CLRS_CLRXY_c => -- 6
                  -- Set X & Y Register to 0  and clear screen
                  XPos_Reg <= (others => '0');
                  YPos_Reg <= (others => '0');
  --                cmd_clrs <= '1';
                  drawCmd     <= clearScreen_e;
                  drawCmd_stb <= '1';
                when cmd_CLRS_CLRALL_c => -- 7
                  -- Clear Screen, reset all registers
                  Reset_regs_p;
                  drawCmd       <= clearScreen_e;
                  drawCmd_stb   <= '1';
                when cmd_PEN_WHITE_c => -- 8
                  -- enable LIGHTPEN & WHITE.
                  -- not supported
                when cmd_PEN_c => -- 9
                  -- enable LIGHTPEN.
                  -- not supported
                when cmd_DRAW5x8_c => -- A
                  -- draw a 5x8 Block according to CSIZE
                  draw_symb_p(false);
                when cmd_DRAW4x4_c => -- B
                  -- draw a 4x4 Block according to CSIZE
                  draw_symb_p(true);
                when cmd_INVERS_c => -- C
                  -- fill screen accordig to bit CTRL1.1
                  -- (dark characters on white background)
                  fillScreen  <= '1';
                  drawCmd     <= clearScreen_e;
                  drawCmd_stb <= '1';
                when cmd_CLRX_c =>  -- D
                  -- reset X-register
                  XPos_Reg <= (others => '0');
                when cmd_CLRY_c => -- E
                  -- reset Y-register
                  YPos_Reg <= (others => '0');
                when cmd_DMA_c => -- F
                  -- DMA
--                  cmd_dma <= '1';
                  drawCmd     <= DMA_e;
                  drawCmd_stb <= '1';
                when others => null;
              end case;
            elsif Cmd_Reg(7 downto 4)=X"1" then
              -- Draw standard Vectors using deltax + deltay Register
              -- sign: command signx  signy
              --       0x11      +      +
              --       0x13      -      +
              --       0x15      +      -
              --       0x17      -      -
              -- => signy = cmd.2; signx = cmd.1
              -- special vectors:  command  DeltaX   DeltaY
              --                   0x10      +       n.a.
              --                   0x12      n.a.    +
              --                   0x14      n.a.    -
              --                   0x16      -       n.a.
              --
              --           010(90°)
              --
              --     011(135°)    001(45°)
              --
              -- 110(180°)            000(0°)
              --
              --     111(225°)   101(315°)
              --           100(270°)
              drawCmd     <= drawLine_e;
              drawCmd_stb <= '1';
  
              if Cmd_Reg(3) ='0' then
                -- std. Vectors
                deltaX <= calc_delta_f(to_integer(unsigned(DeltaX_Reg)),Cmd_Reg(1));
                deltaY <= calc_delta_f(to_integer(unsigned(DeltaY_Reg)),Cmd_Reg(2));
                case Cmd_Reg(2 downto 0) is
                  when "000" | "110" =>
                    -- ignore DeltaY
                    deltaY <= (others => '0');
                  when "010" | "100" =>
                    -- ignore DeltaX
                    deltaX <= (others => '0');
                  when others => null;
                end case;
              else
                -- special Vecors. Ignore smaller register
                delta_v := DeltaX_Reg;
                if unsigned(DeltaY_Reg) > unsigned(DeltaX_Reg) then
                  delta_v := DeltaY_Reg;
                end if;
                deltaX <= calc_delta_f(to_integer(unsigned(delta_v)),Cmd_Reg(1));
                deltaY <= calc_delta_f(to_integer(unsigned(delta_v)),Cmd_Reg(2));
                case Cmd_Reg(2 downto 0) is
                  when "000" | "110" =>
                    -- ignore DeltaY
                    deltaY <= (others => '0');
                  when "010" | "100" =>
                    -- ignore DeltaX
                    deltaX <= (others => '0');
                  when others => null;
                end case;
              end if;
            elsif Cmd_Reg(7)='0' then
              -- draw a symbol
              draw_symb_p(false);
  
            else
              -- small vectors
              -- b6:5 ... |delta x|
              -- b4:3 ... |delta y|
              -- b2:0 ... direction
              drawCmd     <= drawLine_e;
              drawCmd_stb <= '1';
  
              deltaX      <= calc_delta_f(to_integer(unsigned(Cmd_Reg(6 downto 5))),Cmd_Reg(1));
              deltaY      <= calc_delta_f(to_integer(unsigned(Cmd_Reg(4 downto 3))),Cmd_Reg(2));
              case Cmd_Reg(2 downto 0) is
                when "000" | "110" =>
                  -- ignore DeltaY
                  deltaY <= (others => '0');
                when "010" | "100" =>
                  -- ignore DeltaX
                  deltaX <= (others => '0');
                when others => null;
              end case;
            end if;
          end if;
          if ((drawCmd_stb and not Ctrl1_Reg(0))='1' and drawCmd/=clearScreen_e) or
              (draw_finished and Ctrl1_Reg(0))= '1' or
              (draw_finished='1' and drawCmd=clearScreen_e) then
  --          cmd_clrs <= '0';
            if drawCmd/=clearScreen_e and drawCmd/=DMA_e then
              XPos_Reg <= calc_new_pos_f(unsigned(XPos_Reg),signed(deltaX),9,Ctrl1_Reg(3));
              YPos_Reg <= calc_new_pos_f(unsigned(YPos_Reg),signed(deltaY),8,Ctrl1_Reg(3));
            end if;
            drawCmd  <= idle_e;
          end if;
--          if dma_finished ='1' then
--            cmd_dma <= '0';
--          end if;
        end if;
      end if;
    end if;
  end process Regs;

  draw_finished <= not drawBusy_i and drawBusy_d;

  Status_Reg(7 downto 4) <= (others => '0');
  Status_Reg(3)          <= '1' when unsigned(XPos_Reg) >= 512 or
                                     unsigned(YPos_Reg) >= 256 else
                            '0';
  Status_Reg(2)          <= not (drawCmd_stb or drawBusy_i);  -- or not cmd_dma);
  Status_Reg(1)          <= vsync_i;
  Status_Reg(0)          <= '0';



  symbol_o      <= X"80" when Cmd_Reg=(X"0" & cmd_DRAW5x8_c) else
                   X"81" when Cmd_Reg=(X"0" & cmd_DRAW4x4_c) else
                   "0" & Cmd_Reg(6 downto 0);
  scaleX_o      <= CSize_Reg(7 downto 4);
  scaleY_o      <= CSize_Reg(3 downto 0);
  posStartX_o   <= XPos_Reg(posStartX_o'range);
  posStartY_o   <= YPos_Reg(posStartY_o'range);
  deltaX_o      <= deltaX;
  deltaY_o      <= deltaY;
  lineStyle_o   <= Ctrl2_Reg(1 downto 0); -- 
  symbolStyle_o <= Ctrl2_Reg(3 downto 2); -- 2=1 (kusiv), 3=1(vertikal)
  drawCmd_o     <= drawCmd;
  drawCmd_stb_o <= drawCmd_stb when drawCmd=clearScreen_e else
                   drawCmd_stb and Ctrl1_Reg(0);
  color_o       <= '0' when drawCmd=clearScreen_e and fillScreen='0' else
                   Ctrl1_Reg(1);
  vidEnable_o   <= not Ctrl1_Reg(2);
  neverLeave_o  <= Ctrl1_Reg(3);
  irqEn_o       <= Ctrl1_Reg(6 downto 4);
  
  char_rom_addr_o <= XPos_Reg(char_rom_addr_o'range);
  char_rom_we_o   <= (CS_i and Wr_i) when Adr_i(3 downto 0)=X"E" else
                     '0';
  char_rom_page_o <= Ctrl2_Reg(4); -- 1=USER-Zeichensatz
  raster_bg_o     <= Ctrl2_Reg(5); -- 1=Transparent Mode
  
  hwcuren_o <= Ctrl2_Reg(6);	-- 1=hardware cursor enable
  
end rtl;
