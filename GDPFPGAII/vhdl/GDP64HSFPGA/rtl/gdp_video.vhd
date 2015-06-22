--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - Video Unit
-- File        : GDP_video.vhd
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
use work.gdp_global.all;
use ieee.std_logic_unsigned.all;
--use ieee.std_logic_arith.all;


entity gdp_video is
  port(reset_n_i  : in  std_ulogic;
       clk_en_i   : in  std_ulogic;
       clk_i      : in  std_ulogic;
       -----------------------------
       -- interface to VRAM
       -----------------------------
       rd_req_o   : out std_ulogic;
       rd_addr_o  : out std_ulogic_vector(15 downto 0);
       rd_data_i  : in  std_ulogic_vector(7 downto 0);
       rd_ack_i   : in  std_ulogic;
       rd_busy_i  : in  std_ulogic;
       -----------------------------
       scroll_i   : in  std_ulogic_vector(6 downto 0);
       enable_i   : in  std_ulogic;
       -----------------------------
       clut_we_i     : in  std_ulogic;
       clut_addr_i   : in  std_ulogic_vector(3 downto 0);
       clut_data_i   : in  std_ulogic_vector(8 downto 0);
       -----------------------------
       pixel_red_o   : out std_ulogic_vector(2 downto 0);
       pixel_green_o : out std_ulogic_vector(2 downto 0);
       pixel_blue_o  : out std_ulogic_vector(2 downto 0);
       Hsync_o    : out std_ulogic;
       Vsync_o    : out std_ulogic;
	   blank_o		    : out std_ulogic;
	   ------------------------------------------------------------------------
       -- Hardware-Cursor (to VIDEO section)
       ------------------------------------------------------------------------
	   hwcuren_i		: in std_ulogic; -- hardware cursor enable ( CTRL2.6)
	   curcol_i			: in std_ulogic_vector(3 downto 0);	-- current FG color
	   cx1_i			: in std_ulogic_vector(11 downto 0);
	   cx2_i			: in std_ulogic_vector(11 downto 0);
	   cy1_i			: in std_ulogic_vector(11 downto 0);
	   cy2_i			: in std_ulogic_vector(11 downto 0);
	   	   
       --------------------------
       -- Monitoring (Debug) signals
       --------------------------
       monitoring_o: out std_ulogic_vector(7 downto 0)
     );
end gdp_video;

architecture rtl of gdp_video is
  component gdp_clut is
    port (
      reset_n_i    : in  std_ulogic;
      clk_i        : in  std_ulogic; 
      clk_en_i     : in  std_ulogic;
      WrAddress_i  : in  std_ulogic_vector(3 downto 0); 
      Data_i       : in  std_ulogic_vector(8 downto 0); 
      WE_i         : in  std_ulogic; 
      RdAddress_i  : in  std_ulogic_vector(3 downto 0); 
      Data_o       : out std_ulogic_vector(8 downto 0)
    );
  end component;

  constant Stages_c          : natural := 11;
  constant HFRONT_PORCH_c    : natural := 40;
  constant HBACK_PORCH_c     : natural := 88;
  constant HSYNC_c           : natural := 128;
  constant HMAX_c            : natural := HFRONT_PORCH_c+HSYNC_c+HBACK_PORCH_c+800;
  constant VFRONT_PORCH_c    : natural := 1;
  constant VBACK_PORCH_c     : natural := 23;
  constant VSYNC_c           : natural := 4;
  constant VMAX_c            : natural := VFRONT_PORCH_c+VSYNC_c+VBACK_PORCH_c+600; -- 40 + 4 + 23 + 600 = 667
  
  type rd_state_t is(idle_e, wait_ack_e, s1_e, s2_e);
  
  signal q                   : unsigned(Stages_c-1 downto 0); -- Pixel-Takt-Zähler
  signal Line                : unsigned(Stages_c-1 downto 0);
  signal HSYNC, VSYNC, VidEn : std_ulogic;
  signal Pixel               : std_ulogic_vector(3 downto 0);
--  signal Pixel_count         : unsigned(2 downto 0);
--  signal next_Pixel_count    : unsigned(2 downto 0);
  signal rd_data,next_rd_data: std_ulogic_vector(7 downto 0);
  signal set_rd_data         : std_ulogic; 

  signal rd_state,next_rd_state       : rd_state_t;
  signal rd_address                   : unsigned(15 downto 0);
  signal wait_not_busy,rd_req         : std_ulogic;
  signal frame_start                  : std_ulogic;
  signal rgb_pixel                    : std_ulogic_vector(8 downto 0);
  signal clut_q                       : std_ulogic_vector(8 downto 0);
  
  signal isCursor : std_ulogic;
  
begin

  -- http://info.electronicwerkstatt.de/bereiche/monitortechnik/vga/Standard-Timing/
  -- Horizontal (Pixel):
  -- Front Porch 40
  -- Sync 128
  -- Back Porch 88
  -- Vorlauf 144
  -- Aktiv 512
  -- Nachlauf 144
  -- Gesamt 1056
  -- 
  -- Vertikal (Linien):
  -- Front Porch 1
  -- Sync 4
  -- Back Porch 23
  -- Vorlauf 44
  -- Aktiv 512
  -- Nachlauf 44
  -- Gesamt 628
  
  --             0           512       656   696  824   912       1056         
  --                                    S  40   128  88
  -- Horizontal: ---- 512 ----|---144---| HFP | HS | HBP |---144---| 
  --             0           512       556  557   561   584       628
  --                                               fs
  --                                    S   1   4     23
  -- Vertical:   ---- 512 ----|---44----| VFP | VS | VBP |---44----|
  
  
--  VidEn <= '1' when q >= to_unsigned(HSYNC_c+HBACK_PORCH_c+144, Stages) and 
--                    q < to_unsigned(HSYNC_c+HBACK_PORCH_c+656 , Stages)
--               and Line >= to_unsigned(VBACK_PORCH_c, Stages) and 
--                   Line < to_unsigned(VBACK_PORCH_c+512, Stages)
--           else '0';
  VidEn <= '1' when q < 512 and Line < 512 else
           '0';
  -- blank generation 
  -- blank_o <= '1' when (Line > 515 and Line < VMAX_c - 2) or (q > 515 and q < HMAX_c - 2) else '0';
  --blank_o <= '1' when (Line > 513 and Line < 627) or (q > 513 and q < 1055) else '0';
  blank_o <= '1' when (Line > 556 and Line < 561) else '0';
  --blank_o <= '1' when (q > 513 and q < 1050) else '0';
  
  
  
  vid : process(clk_i, reset_n_i,cx1_i,cx2_i,cy1_i,cy2_i,hwcuren_i) 						-- Generierung von VSync, HSync etc.
  begin
    if reset_n_i = '0' then
      HSYNC       <= '0'; 
      Line        <= to_unsigned(512 + 44 + VFRONT_PORCH_c-1, Stages_c);
      VSYNC       <= '0';
      q           <= (others => '0');
      frame_start <= '0';
--      line_start  <= '0'; 
    elsif rising_edge(clk_i) then
      if clk_en_i = '1' then
        frame_start <= '0';
  --      line_start  <= '0'; 
        q           <= q + 1;								-- Pixel-Zähler mit jedem Clock eins weiter
				
	   -- detect hardware cursor:			
	   if(hwcuren_i = '1') then		
		if(std_logic_vector(q) >= std_logic_vector(cx1_i(10 downto 0))) then
		 if(std_logic_vector(q) < std_logic_vector(cx2_i(10 downto 0))) then
		  if(std_logic_vector(Line) >= std_logic_vector(cy1_i(10 downto 0))) then
		   if(std_logic_vector(Line) < std_logic_vector(cy2_i(10 downto 0))) then
				isCursor <= '1';
		    else
				isCursor <='0';
		    end if;
		   else
		     isCursor <='0';
		   end if;
		  else
		     isCursor <='0';
		  end if;
		 else
		    isCursor <='0';
		 end if;
		else
          isCursor <='0';
        end if;		  
		  
		
        if q = to_unsigned(HMAX_c-1, Stages_c) then			-- und nach Zeilen-Ende wieder auf 0
          q    <= (others => '0');
  --      elsif q = to_unsigned(HSYNC_c-1, Stages) then
        elsif q = to_unsigned(512 + 144 + HFRONT_PORCH_c-1, Stages_c) then
          HSYNC <= '1'; -- activate HSync					-- HSync setzen und Zeile um 1 erhöhen
          Line       <= Line + 1;
  --        line_start <= '1';
  --        if Line = to_unsigned(VMAX_c-VSYNC_c-1, Stages) then
          if Line = to_unsigned(512 + 44 + VFRONT_PORCH_c-1, Stages_c) then
            VSYNC <= '1'; -- activate VSync					-- VSync setzen
          elsif Line = to_unsigned(512 + 44 + VFRONT_PORCH_c + VSYNC_c -1, Stages_c) then
            VSYNC       <= '0';								-- nach VSyncPhase VSync löschen und frame_start setzen					
            frame_start <= '1';
          elsif Line = to_unsigned(VMAX_c-1, Stages_c) then
            Line <= (others => '0');						-- Am Bildende Line wieder auf 0
          end if;
          
        elsif q = to_unsigned(512 + 144 + HFRONT_PORCH_c + HSYNC_c -1, Stages_c) then
          HSYNC <= '0';										-- nach HSyncPhase HSync wieder auf 0
        end if;
		
      end if;
    end if;
  end process vid;


  RD_FSM_COMB: process(rd_state,rd_data_i,rd_address, rd_data,
                       rd_ack_i,rd_busy_i, VidEn, q, frame_start, 
                       HSYNC,VSYNC,enable_i)
  begin
    next_rd_state       <= rd_state;
--    next_Pixel_count    <= Pixel_count;
    next_rd_data        <= (others => '-');
    set_rd_data         <= '0';
    rd_req              <= '0';

    case rd_state is       
      when idle_e =>
        -- currently no data available. do a prefetch
        if frame_start = '1' then
          rd_req        <= '1';
          next_rd_state <= wait_ack_e;
        end if;
        
      when wait_ack_e =>
        if rd_busy_i='0' then
          next_rd_state    <= s1_e;
          set_rd_data      <= '1';
          next_rd_data     <= rd_data_i;
--          next_Pixel_count <= (others => '0');
--          rd_req           <= '1';
        end if;
        
      when s1_e => 
        rd_req           <= '1';
        next_rd_state    <= s2_e;
        
      when s2_e =>  
        -- process odd pixel (15 downto 8)      
        if VidEn = '1' then
          if (not HSYNC and not VSYNC) = '1' then
            if not color_support_c then
              if unsigned(q(2 downto 0)) = 7 then
                set_rd_data      <= '1';
                next_rd_data     <= rd_data_i;
                rd_req           <= '1';
              else
                set_rd_data     <= '1';
                next_rd_data    <= std_ulogic_vector(shift_left(unsigned(rd_data),1)); -- 8 pixel per byte 
              end if;
            else
              if q(0)='1' then
                set_rd_data      <= '1';
                next_rd_data     <= rd_data_i;
                rd_req           <= '1';
              else
                set_rd_data              <= '1';
                next_rd_data(7 downto 4) <= rd_data(3 downto 0); -- 2 pixel per Byte
              end if;
            end if;
          end if;
        end if;     
        if VSYNC = '1' then
          next_rd_state   <= idle_e;
          rd_req          <= '0';
        end if;
                
      when others =>
        next_rd_state <= idle_e;
    end case;
    if enable_i='0' then
      next_rd_state <= idle_e;
    end if;    
  end process;
  
  process(clk_i,reset_n_i)
  begin
    if reset_n_i ='0' then
      rd_state       <= idle_e;
      if color_support_c then
        rd_address     <= to_unsigned(255*512/2,rd_address'length); -- last line
      else
        rd_address     <= to_unsigned(255*512/8,rd_address'length); -- last line
      end if;
      rd_data        <= (others => '0');
--      Pixel_count    <= (others => '0');
--      pixel          <= '0';
      wait_not_busy  <= '0';
    elsif rising_edge(clk_i) then
      if clk_en_i = '1' then
        rd_state       <= next_rd_state;
        if set_rd_data = '1' then
          rd_data      <= next_rd_data;
        end if;
  --      pixel          <= next_pixel;
  --      Pixel_count    <= next_Pixel_count;
        if enable_i = '1' then        
          if rd_req='1' then
            wait_not_busy<='1';
            if color_support_c then
              if rd_address(7 downto 0) = "11111111" then
                rd_address(7 downto 0) <= "00000000";
                if Line(0) = '1' then
                  rd_address(rd_address'high downto 8) <= rd_address(rd_address'high downto 8) - 1;
                end if;
              else
                rd_address(7 downto 0) <= rd_address(7 downto 0) + 1;
              end if;
            end if;
          elsif wait_not_busy='1' and rd_busy_i='0' then
            wait_not_busy <='0';
            if not color_support_c then
              if rd_address(5 downto 0) = "111111" then
                rd_address(5 downto 0) <= "000000";
                if Line(0) = '1' then
                  rd_address(rd_address'high-2 downto 6) <= rd_address(rd_address'high-2 downto 6) - 1;
                end if;
              else
                rd_address(5 downto 0) <= rd_address(5 downto 0) + 1;
              end if;
--            else
--              if rd_address(7 downto 0) = "11111111" then
--                rd_address(7 downto 0) <= "00000000";
--                if Line(0) = '1' then
--                  rd_address(rd_address'high downto 8) <= rd_address(rd_address'high downto 8) - 1;
--                end if;
--              else
--                rd_address(7 downto 0) <= rd_address(7 downto 0) + 1;
--              end if;
            end if;
          elsif wait_not_busy='0' and rd_req='0' and rd_busy_i='0' and VSYNC='1' then
            if color_support_c then
              rd_address     <= to_unsigned(255*512/2,rd_address'length); -- last line
            else
              rd_address     <= to_unsigned(255*512/8,rd_address'length); -- last line
            end if;
    --        rd_address <= (others => '0');
          end if;
        else
          wait_not_busy <= '0';
          if color_support_c then
            rd_address          <= to_unsigned(255*512/2,rd_address'length); -- last line
            rd_data(7 downto 4) <= "0000";
          else
            rd_address <= to_unsigned(255*512/8,rd_address'length); -- last line
            rd_data(7) <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;
  
  color_pixel: if color_support_c generate
  Pixel    <= rd_data(7 downto 4) when isCursor = '0' else
				curcol_i;			  
  end generate;
  
  no_color_pixel: if not color_support_c generate
  Pixel    <= "000" & rd_data(7) when isCursor = '0' else
              "0001";
  end generate;
			  
  rd_req_o <= rd_req;
  rd_addr_o<= std_ulogic_vector(rd_address + (unsigned(scroll_i) & "000000000")) when color_support_c else
              "00" & std_ulogic_vector(rd_address(13 downto 0) + (unsigned(scroll_i) & "0000000"));

  no_clut: if not use_clut_c or not color_support_c generate
    process(clk_i)
      function lookup(color : in std_ulogic_vector(3 downto 0)) return std_ulogic_vector is
        variable tmp : std_ulogic_vector(8 downto 0);
      begin
        tmp := (others => '0'); 
-- pragma translate_off
        if not is_x(color) then
-- pragma translate_on      
          case to_integer(unsigned(color)) is
            when 0  => tmp := "000000000"; -- 0  Schwarz        => 0   RGB 0,0,0
            when 1  => tmp := "111111111"; -- 1  Weiß           => 15  RGB 255,255,255
            when 2  => tmp := "111111000"; -- 2  Gelb           => 14  RGB 255,255,0
            when 3  => tmp := "000111000"; -- 3  Grün           => 10  RGB 0,255,0
            when 4  => tmp := "111000000"; -- 4  Rot            => 12  RGB 255,0,0
            when 5  => tmp := "000000111"; -- 5  Blau           => 9   RGB 0,0,255
            when 6  => tmp := "111000111"; -- 6  Violett        => 13  RGB 255,0,255
            when 7  => tmp := "000111111"; -- 7  Zyan           => 11  RGB 0,255,255
            when 8  => tmp := "001001001"; -- 8  Dunkelgrau     => 8   RGB 64,64,64
            when 9  => tmp := "100100100"; -- 9  Hellgrau       => 7   RGB 128,128,128
            when 10 => tmp := "011011000"; -- 10 Dunkelgelb     => 6   RGB 96,96,0
            when 11 => tmp := "000011000"; -- 11 Dunkelgrün     => 2   RGB 0,96,0
            when 12 => tmp := "011000000"; -- 12 Dunkelrot      => 4   RGB 96,0,0
            when 13 => tmp := "000000011"; -- 13 Dunkelblau     => 1   RGB 0,0,96
            when 14 => tmp := "011000011"; -- 14 Violett dunkel => 5   RGB 96,0,96
            when 15 => tmp := "000011011"; -- 15 Zyan dunkel    => 3   RGB 0,96,96
            when others => null;
          end case;
-- pragma translate_off
        end if;
-- pragma translate_on      
        return tmp;
      end;
    begin
      if rising_edge(clk_i) then
        if clk_en_i = '1' then
          if VidEn='1' then
            rgb_pixel <= lookup(Pixel);
          else
            rgb_pixel <= (others => '0');
          end if;
          Hsync_o <= HSYNC;
          Vsync_o <= VSYNC;
        end if;
      end if;
    end process;
  end generate;
  
  use_clut: if use_clut_c and color_support_c generate
  process(clk_i)
  begin
      if rising_edge(clk_i) then
        if clk_en_i = '1' then
          if (VidEn and enable_i)='1' then
            rgb_pixel <= std_ulogic_vector(clut_q);
          else
            rgb_pixel <= (others => '0');
          end if;
          Hsync_o <= HSYNC;
          Vsync_o <= VSYNC;
        end if;
      end if;
    end process;

    clut_inst : gdp_clut
      port map(
        reset_n_i   => reset_n_i,
        clk_i       => clk_i,
        clk_en_i    => clk_en_i,
        WrAddress_i => clut_addr_i,
        Data_i      => clut_data_i,
        WE_i        => clut_we_i,
        RdAddress_i => Pixel,
        Data_o      => clut_q
      );
  end generate;  
  
  pixel_red_o    <= rgb_pixel(8 downto 6);
  pixel_green_o  <= rgb_pixel(5 downto 3);
  pixel_blue_o   <= rgb_pixel(2 downto 0);

  --with rd_state select
    --monitoring_o(1 downto 0) <= "00" when idle_e,
                                --"01" when wait_ack_e,
                                --"10" when s1_e,
                                --"11" when s2_e;
  --monitoring_o(2) <= VidEn;
  --monitoring_o(3) <= set_rd_data;
  --monitoring_o(4) <= wait_not_busy;
  --monitoring_o(5) <= enable_i;
  --monitoring_o(6) <= HSYNC;
  --monitoring_o(7) <= VSYNC;
  
end rtl;




