--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - Bresenham drawing module
-- File        : gdp_bresenham.vhd
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

entity gdp_bresenham is

  port(clk_i        : in  std_ulogic;
       clk_en_i     : in  std_ulogic;
       reset_n_i    : in  std_ulogic;
       enable_i     : in  std_ulogic;
       ----------------------------------------------------------------------------------
       -- to command module (decoder)
       ----------------------------------------------------------------------------------
       -- start point
       x1_i        : in  coord_t;
       y1_i        : in  coord_t;
       -- end point
       dx_i        : in  delta_t;
       dy_i        : in  delta_t;
       --
       color_i     : in  std_ulogic;
       cmd_stb_i   : in  std_ulogic;
       -- linestyle
       linestyle_i   : in  linestyle_t;

       -- busy strobe for decoder, indicates that module is working
       busy_o      : out std_ulogic;

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
       --------------------------
       -- Monitoring (Debug) signals
       --------------------------
       monitoring_o: out std_ulogic_vector(7 downto 0)
       );

end gdp_bresenham;

architecture RTL of gdp_bresenham is

   -- state definition
  type bres_state_t is (idle_e, line_setup_e, draw_line_e, delay_e, finish_e);

  -- state signals
  signal state, next_state           : bres_state_t;

  -- current x and y position
  signal posx, next_posx : coord_t;
  signal set_posx        : std_ulogic;
  signal posy, next_posy : coord_t;
  signal set_posy        : std_ulogic;
  
  signal x2s,y2s     : coord_t;

  -- delta signals, one bit larger than coords
--  signal dx, next_dx : std_ulogic_vector(posWidth_c downto 0);
--  signal dy, next_dy : std_ulogic_vector(posWidth_c downto 0);

  -- decision variable
  signal e    , next_e    : std_ulogic_vector(deltaWidth_c downto 0);
  signal set_e            : std_ulogic;
  signal einc , next_einc : std_ulogic_vector(deltaWidth_c downto 0);
  signal edec , next_edec : std_ulogic_vector(deltaWidth_c downto 0);

  -- first signal (for exeption on first pixel)
  signal first, next_first : std_ulogic;

  -- signals for drawing direction determination
  signal neg_slope_x, next_neg_slope_x : std_ulogic;
  signal neg_slope_y, next_neg_slope_y : std_ulogic;

  -- store rising edge of valid signal
--  signal valid_del1, valid_rising : std_ulogic;

  -- flag storing wheather the slope is grater or smaller than 1
  signal slope_greater1, next_slope_greater1 : std_ulogic;

  -- resulting linestyle according to the input register
  signal linestyle : std_ulogic_vector(0 to 7);

  -- linestyle counter
  signal linestyle_count, next_linestyle_count : unsigned(3 downto 0);
  signal set_ls_count                          : std_ulogic;
  -- write request
  signal next_wr_req : std_ulogic;
  signal wr_req      : std_ulogic;

  -- command acknowledge expected signal
  signal ack_expected, next_ack_expected : std_ulogic;
  signal linestyle_pixel                 : std_ulogic;

  -- force write
--  signal next_last_wr : std_ulogic;

 
begin

    -----------------------------------------------------------------------------
    -- FSM registers
    -----------------------------------------------------------------------------
    fsm_regs: process (clk_i, reset_n_i)
    begin
      if reset_n_i = ResetActive_c then
        state            <= idle_e;
--        dx               <= (others => '0');
--        dy               <= (others => '0');
        e                <= (others => '0');
        einc             <= (others => '0');
        edec             <= (others => '0');
        posx             <= (others => '0');
        posy             <= (others => '0');
        first            <= inactivated_c;
        neg_slope_x      <= inactivated_c;
        neg_slope_y      <= inactivated_c;
--        valid_rising     <= inactivated_c;
--        valid_del1       <= inactivated_c;
        slope_greater1   <= inactivated_c;
        linestyle_count  <= (others => '0');
        wr_req           <= inactivated_c;
        ack_expected     <= inactivated_c;
--        finished         <= inactivated_c;
--        last_wr_o        <= inactivated_c;
      elsif rising_edge(clk_i) then
        if clk_en_i = '1' then
          state            <= next_state;
  
          if set_e=activated_c then
            e              <= next_e;
          end if;
          
  
  
          if set_posx = activated_c then
            posx           <= next_posx;
          end if;
          if set_posy = activated_c then
            posy           <= next_posy;
          end if;
          first            <= next_first;
          neg_slope_x      <= next_neg_slope_x;
          neg_slope_y      <= next_neg_slope_y;
  
          if state = line_setup_e then
            einc           <= next_einc;
            edec           <= next_edec;
            slope_greater1 <= next_slope_greater1;
          end if;
          if set_ls_count = '1' then
            linestyle_count  <= next_linestyle_count;
          end if;
  
          wr_req           <= next_wr_req;
          ack_expected     <= next_ack_expected;
--          last_wr_o        <= next_last_wr;
        end if;
      end if;
    end process fsm_regs;
    
--    color_wr_req_o   <= wr_req;
    wr_req_o   <= wr_req;

--    xincr <= "01" when to_integer(signed(dx_i)) >= 0 else
--             "11";
--    yincr <= "01" when to_integer(signed(dy_i)) >= 0 else
--             "11";
    
--    x2s <= std_ulogic_vector(signed(x1_i) + to_integer(signed(dx_i)) - xincr);
--    y2s <= std_ulogic_vector(signed(y1_i) + to_integer(signed(dy_i)) - yincr);
    x2s <= std_ulogic_vector(signed(x1_i) + to_integer(signed(dx_i)));
    y2s <= std_ulogic_vector(signed(y1_i) + to_integer(signed(dy_i)));
    
    -----------------------------------------------------------------------------
    -- Determine next state and output
    -----------------------------------------------------------------------------
    fsm_comb: process (ack_expected, dx_i, dy_i, e, edec, einc, first,
                       linestyle_pixel, linestyle_count, neg_slope_x, neg_slope_y,
                       posx, posy, slope_greater1, state, cmd_stb_i,
                       x2s,y2s,
                       wr_ack_i, x1_i,y1_i, 
                       enable_i,wr_req)
      variable dx_v, dy_v          : delta_t;
      variable slopex_v,slopey_v   : signed(1 downto 0);
    begin
    
      -- default assignments
      next_state           <= state;
      next_posx            <= (others => '-');
      set_posx             <= inactivated_c;
      next_posy            <= (others => '-');
      set_posy             <= inactivated_c;
--      next_dx              <= dx;
--      next_dy              <= dy;
      next_e               <= (others => '-');
      set_e                <= inactivated_c;
      next_einc            <= (others => '-');
      next_edec            <= (others => '-');
      busy_o               <= activated_c;
      next_first           <= first;
      next_neg_slope_x     <= neg_slope_x;
      next_neg_slope_y     <= neg_slope_y;
      next_slope_greater1  <= '-';
      next_linestyle_count <= (others => '-');
      set_ls_count         <= '0';
      next_wr_req          <= inactivated_c;
      next_ack_expected    <= ack_expected;
--      next_last_wr         <= inactivated_c;
      dx_v                 := std_ulogic_vector(abs(signed(dx_i)));
      dy_v                 := std_ulogic_vector(abs(signed(dy_i)));
      
      case state is
        -----------------------------------------------------------------------
        -- Determine wheather line, rectangle or pixel
        -- shall be drawn and do some setup
        -----------------------------------------------------------------------
        when idle_e =>
          busy_o            <= inactivated_c;
          next_ack_expected <= inactivated_c;
          
    
          -- wait until inputs are flagged valid and
          -- last write access is completed
          if (enable_i and cmd_stb_i) = activated_c then 
            -- clear linestyle counter
            set_ls_count         <= '1';
            next_linestyle_count <= (others => '0');
            -- decide which command shall be executed
            -- determine drawing direction
            if (signed(x1_i) = signed(x2s)) and
               (signed(y1_i) = signed(y2s)) then
                -- Set the write request and the indication that this
                -- is the only data word.
                next_wr_req    <= activated_c;
                next_ack_expected <= activated_c;
--                next_last_wr   <= activated_c;
                busy_o         <= activated_c;
                set_posx       <= activated_c;
                next_posx      <= x1_i;
                set_posy       <= activated_c;
                next_posy      <= y1_i;
                -- don't wait for write acknowledge, go immediately
                -- back to idle state, also no busy flag is set
                next_state     <= delay_e;      
            -- ******************** rectangle filling ********************
            else
              if signed(x1_i) > signed(x2s) then
                next_neg_slope_x <= activated_c;
              else
                next_neg_slope_x <= inactivated_c;
              end if;
              if signed(y1_i) > signed(y2s) then
                next_neg_slope_y <= activated_c;
              else
                next_neg_slope_y <= inactivated_c;
              end if;
              
              -- calculate delta values in x and y direction for line drawing
              -- dx = x2 - x1              
--              next_dx <= std_ulogic_vector(abs(signed(resize(signed(x2s), dx'length)) - resize(signed(x1_i), dx'length)));
--              -- dy = y2 - y1
--              next_dy <= std_ulogic_vector(abs(signed(resize(signed(y2s), dy'length)) - resize(signed(y1_i), dy'length)));
              -- bresenham line starting point is always (x1,y1)
              set_posx  <= activated_c;
              next_posx <= x1_i;
              set_posy  <= activated_c;
              next_posy <= y1_i;
              -- proceed to setup state
              next_state <= line_setup_e;
            end if;
          end if;

        -----------------------------------------------------------------------
        -- Setup calculation for line drawing (once per line)
        -----------------------------------------------------------------------
        when line_setup_e =>
          -- decide wheather slope is greater or smaller than 1
--          if abs(signed(dx_i)) >= abs(signed(dy_i)) then
          if signed(dx_v) >= signed(dy_v) then
            -- x is the independent variable (always incremented)
            -- e = -dx
            set_e  <= activated_c;
            next_e <= std_ulogic_vector(signed'(signed(not(resize(signed(dx_v), e'length))) + 1 ));
--            next_e <= std_ulogic_vector(signed'(signed(not(std_logic_vector'(sxt(std_logic_vector(dx), e'length)))) + 1 ));
            -- edec = 2*dy
            next_edec <= dy_v&"0";
            -- einc = 2*(dy-dx)
            next_einc <= std_ulogic_vector(signed'(signed(dy_v) - signed(dx_v))) & "0";
            next_slope_greater1 <= inactivated_c;
          else
            -- y is the independent variable (always incremented)
            -- e = -dy
            set_e  <= activated_c;
            next_e <= std_ulogic_vector(signed'(signed(not(resize(signed(dy_v), e'length))) + 1 ));
            -- edec = 2*dx
            next_edec <= dx_v&"0";
            -- einc = 2*(dx-dy)
            next_einc <= std_ulogic_vector(signed'(signed(dx_v) - signed(dy_v))) & "0";
            next_slope_greater1 <= activated_c;
          end if;
          -- proceed to calculation state
          next_state <= draw_line_e;
          next_first <= activated_c;


        -----------------------------------------------------------------------
        -- Calculate next pixel of line and issue single request
        -----------------------------------------------------------------------
        when draw_line_e =>
          slopex_v := (others => '0');
          slopey_v := (others => '0');
          next_first <= inactivated_c;
          -- wait until acknowledge of last request occurs
          if ack_expected = inactivated_c  or wr_ack_i = activated_c then
            next_ack_expected <= inactivated_c;            
            
            -- end of line ?
            if (signed(posx) = signed(x2s) and  slope_greater1 = inactivated_c) or
               (signed(posy) = signed(y2s) and  slope_greater1 = activated_c) then
              next_state   <= delay_e;    
--              next_last_wr <= activated_c;
            else
              -- x/y position shall not be incremented for the first pixel
              if first = inactivated_c then
                -- increment the independent variable
                if neg_slope_x = activated_c and neg_slope_y = activated_c then
                  if slope_greater1 = inactivated_c then
                    set_posx  <= activated_c;
--                    next_posx <= std_ulogic_vector(signed'(signed(posx) - 1));
                    slopex_v := to_signed(-1,slopex_v'length);
                  else
                    set_posy  <= activated_c;
--                    next_posy <= std_ulogic_vector(signed'(signed(posy) - 1));
                    slopey_v := to_signed(-1,slopex_v'length);
                  end if;
                elsif neg_slope_x = activated_c and neg_slope_y = inactivated_c then
                  if slope_greater1 = inactivated_c then
                    set_posx  <= activated_c;
--                    next_posx <= std_ulogic_vector(signed'(signed(posx) - 1));
                    slopex_v := to_signed(-1,slopex_v'length);
                  else
                    set_posy  <= activated_c;
--                    next_posy <= std_ulogic_vector(signed'(signed(posy) + 1));
                    slopey_v := to_signed(1,slopey_v'length);
                  end if;
                elsif neg_slope_x = inactivated_c and neg_slope_y = activated_c then
                  if slope_greater1 = inactivated_c then
                    set_posx  <= activated_c;
--                    next_posx <= std_ulogic_vector(signed'(signed(posx) + 1));
                    slopex_v := to_signed(1,slopex_v'length);
                  else
                    set_posy  <= activated_c;
--                    next_posy <= std_ulogic_vector(signed'(signed(posy) - 1));
                    slopey_v := to_signed(-1,slopey_v'length);
                  end if;
                else
                  if slope_greater1 = inactivated_c then
                    set_posx  <= activated_c;
--                    next_posx <= std_ulogic_vector(signed'(signed(posx) + 1));
                    slopex_v := to_signed(1,slopex_v'length);
                  else
                    set_posy  <= activated_c;
--                    next_posy <= std_ulogic_vector(signed'(signed(posy) + 1));
                    slopey_v := to_signed(1,slopey_v'length);
                  end if;
                end if;
              end if;

               -- update decision signal and increment depending variable
              if signed(e) <= 0 then
                -- e = e + edec
                set_e  <= activated_c;
                next_e <= std_ulogic_vector(signed'(signed(e) + signed(edec)));
              else
                -- e = e + einc
                set_e  <= activated_c;
                next_e <= std_ulogic_vector(signed'(signed(e) + signed(einc)));
                if first = inactivated_c then          
                  -- increment the depending variable
                  if slope_greater1 = inactivated_c then
                    if neg_slope_y = activated_c then
                      set_posy  <= activated_c;
--                      next_posy <= std_ulogic_vector(signed'(signed(posy) - 1));
                      slopey_v := to_signed(-1,slopey_v'length);
                    else
                      set_posy  <= activated_c;
--                      next_posy <= std_ulogic_vector(signed'(signed(posy) + 1));
                      slopey_v := to_signed(1,slopey_v'length);
                    end if;
                  else
                    if neg_slope_x = activated_c then
                      set_posx  <= activated_c;
--                      next_posx <= std_ulogic_vector(signed'(signed(posx) - 1));
                      slopex_v := to_signed(-1,slopex_v'length);
                    else
                      set_posx  <= activated_c;
--                      next_posx <= std_ulogic_vector(signed'(signed(posx) + 1));
                      slopex_v := to_signed(1,slopex_v'length);
                    end if;
                  end if;
                end if;
              end if;
              next_posx <= std_ulogic_vector(signed'(signed(posx) + slopex_v));
              next_posy <= std_ulogic_vector(signed'(signed(posy) + slopey_v));
              -- increment linestyle counter
              set_ls_count         <= '1';
              next_linestyle_count <= linestyle_count+1;
              -- proceed to deliver exept linestyle prohibits this
--              if linestyle(to_integer(linestyle_count(3 downto 1))) = '1' then
              if linestyle_pixel = '1' then
                -- deliver the next_pixel
                next_wr_req       <= activated_c;
                next_ack_expected <= activated_c;
              else
                next_state <= draw_line_e;
              end if;
            end if;
          end if;

        when delay_e =>
          next_state <= finish_e;
          
        when finish_e =>
--          if busy_wr_i='0' and wr_req='0' then
          if wr_req='0' and 
             (ack_expected = inactivated_c  or wr_ack_i = activated_c) then
            next_state <= idle_e;
            busy_o     <= inactivated_c;
          end if;                           
      end case;

      if enable_i = inactivated_c then
        next_state <= idle_e;
      end if;
    
    end process fsm_comb;

    -- linestyle multiplexer
    linestyle <= lineStyles_c(to_integer(unsigned(linestyle_i)));
    
    linestyle_pixel <= linestyle(to_integer(linestyle_count(3 downto 1)));
    
    -- build pixel
--    wr_data_o(rgb_color_t'range)                                         <= color_i;
--    wr_data_o(alpha_t'high+rgb_color_t'high+1 downto rgb_color_t'high+1) <= alpha_i;
    wr_pixel_o <= color_i;
    
    posx_o    <= posx;
    posy_o    <= posy;
  
  with state select
    monitoring_o(2 downto 0) <=  "000" when idle_e,
                                 "001" when line_setup_e,
                                 "010" when draw_line_e,
                                 "011" when delay_e,
                                 "100" when finish_e,
                                 "111" when others;
    monitoring_o(3) <= first;
    monitoring_o(4) <= ack_expected;
    monitoring_o(5) <= neg_slope_x;
    monitoring_o(6) <= neg_slope_y;
    monitoring_o(7) <= linestyle_pixel;
end rtl;
