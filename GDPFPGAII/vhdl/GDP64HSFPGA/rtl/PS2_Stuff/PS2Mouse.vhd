--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : PS/2 Keyboard - LowLevel Transmission
-- File        : PS2Keyboard-a.vhd
-- Description : Architecture for attaching a PS/2 Keyboard to NKC.
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2003 by Andreas Voggeneder
--------------------------------------------------------------------------------


--Command transmission to the keyboard is initiated by bringing the keyboard CLOCK line LOW
--for at least 60 uS. After the 60 uS delay, the DATA line should be brought LOW and the CLOCK 
--line released (HIGH). Make sure to bring the DATA line LOW before releasing the CLOCK line. 
--Some time later (up to 10 milliseconds) the keyboard will start to generate clock signals. 
--At each HIGH to LOW clock transition the keyboard will clock in a new bit.

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.DffGlobal.all;
use work.gdp_global.all;

entity PS2Mouse is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    --------------------------
    -- PS/2 Keyboard signals
    --------------------------
    -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
    -- PS/2 bus. In all other cases an input would be sufficient.
    Ps2Clk_io    : inout std_logic;
    -- PS/2 data line. Bidirectional for reading and writing data.
    Ps2Dat_io    : inout std_logic;
    ----------------------------------
    -- Data Bus
    ----------------------------------
    Adr_i     : in  std_ulogic_vector(2 downto 0);
    en_i      : in  std_ulogic;
    DataIn_i  : in  std_ulogic_vector(7 downto 0);
    Rd_i      : in  std_ulogic;
    Wr_i      : in  std_ulogic;
    DataOut_o : out std_ulogic_vector(7 downto 0)
    --------------------------
    -- Monitoring (Debug) signals
    --------------------------
    --monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
  );
end PS2Mouse;

architecture rtl of PS2Mouse is
  

--  component InputSync
--    generic(levels_g     : natural :=2;
--            ResetValue_g : std_ulogic := '0');
--    port (
--      Input : in  std_ulogic;
--      clk   : in  std_ulogic;
--      clr_n : in  std_ulogic;
--      q     : out std_ulogic);
--  end component;
  component PS2_Interface
    port(
      reset_n_i    : in  std_ulogic;
      clk_i        : in  std_ulogic;
      Ps2Clk_io    : inout std_logic;
      Ps2Dat_io    : inout std_logic;
      data_o       : out std_ulogic_vector(7 downto 0);
      data_stb_o   : out std_ulogic;
      ack_o        : out std_ulogic;
      error_stb_o  : out std_ulogic;
      data_i       : in  std_ulogic_vector(7 downto 0);
      data_stb_i   : in  std_ulogic
    );
  end component;
--  constant transInit_c  : natural :=2400;    -- 60 uS Time to hold CLK low (to initiate a Transmission)
--  constant transStart_c : natural :=2480;    -- 2 uS additonal Time where Data and Clockline are both LOW
--  type   Ps2Direction_t is (FromDevice, ToDevice);
--  type   state_t        is (IDLE, DELAY,START, DATA, STOP);
--  type   clkEdge_t      is (NONE, RISING, FALLING);
 
  type mouse_state_t  is (IDLE_e, RESET_MOUSE_e, READ_ID_e, INIT_e);
  type mouse_send_state_t  is (IDLE_e, WAIT_ACK_e);
  

  ------------------------------
  -- Host to mouse commands

  constant SET_RESOLUTION_c       : std_ulogic_vector(7 downto 0) := X"E8";
  constant ECHO_CODE_c            : std_ulogic_vector(7 downto 0) := X"EE";
  constant GET_DEVICE_ID_c        : std_ulogic_vector(7 downto 0) := X"F2";
  constant SET_SAMPLE_RATE_c      : std_ulogic_vector(7 downto 0) := X"F3";
  constant DATA_REPORT_ENABLE_c   : std_ulogic_vector(7 downto 0) := X"F4";
  constant DATA_REPORT_DISABLE_c  : std_ulogic_vector(7 downto 0) := X"F5";
  constant SET_DEFAULT_CODE_c     : std_ulogic_vector(7 downto 0) := X"F6";
  
  constant RESOLUTION1cmm_c       : std_ulogic_vector(7 downto 0) := X"00";
  constant RESOLUTION2cmm_c       : std_ulogic_vector(7 downto 0) := X"01";
  constant RESOLUTION4cmm_c       : std_ulogic_vector(7 downto 0) := X"02";
  constant RESOLUTION8cmm_c       : std_ulogic_vector(7 downto 0) := X"03";
  
  constant SAMPLERATE_10sps_c     : std_ulogic_vector(7 downto 0) := X"0A";
  constant SAMPLERATE_20sps_c     : std_ulogic_vector(7 downto 0) := X"14";
  constant SAMPLERATE_40sps_c     : std_ulogic_vector(7 downto 0) := X"28";
  constant SAMPLERATE_60sps_c     : std_ulogic_vector(7 downto 0) := X"3C";
  constant SAMPLERATE_80sps_c     : std_ulogic_vector(7 downto 0) := X"50";
  constant SAMPLERATE_100sps_c    : std_ulogic_vector(7 downto 0) := X"64";
  constant SAMPLERATE_200sps_c    : std_ulogic_vector(7 downto 0) := X"C8";

  
  constant PWRON_CODE_c : std_ulogic_vector(7 downto 0) := X"AA";
  constant ACK_CODE_c   : std_ulogic_vector(7 downto 0) := X"FA";
  constant RESET_CODE_c : std_ulogic_vector(7 downto 0) := X"FF";
  
  type Mouse_Init_ARRAY_t is array(0 to 4) of std_ulogic_vector(7 downto 0);
  constant Mouse_Init_ARRAY_c : Mouse_Init_ARRAY_t := (
    SET_SAMPLE_RATE_c, 
    SAMPLERATE_40sps_c,
    SET_RESOLUTION_c,
    RESOLUTION2cmm_c,
    DATA_REPORT_ENABLE_c
  );
  
--  signal Ps2Direction,NextPs2Direction     : Ps2Direction_t;
--  signal state, nextState                  : state_t;
--  signal ps2ClkEdge                        : clkEdge_t;
--  signal q                                 : std_ulogic_vector(3 downto 0);
--  signal Ps2ClkOld                         : std_ulogic;
--  signal sReg                              : std_ulogic_vector(8 downto 0);
--  signal delayCnt                          : unsigned (11 downto 0);
--  signal Ps2Clk,Ps2ClockHtranslate1        : std_ulogic;
--  signal Ps2DataOut, Ps2ClockOut           : std_ulogic;
  signal DataReg                           : std_ulogic_vector(8 downto 0);
  signal set_CmdReg                        : std_ulogic;
  signal next_CmdReg                       : std_ulogic_vector(7 downto 0);
--  signal ackReceived                       : std_ulogic;       
--  signal Reset                             : std_ulogic;

  signal decoder_enable                    : std_ulogic;
  signal mouse_state,next_mouse_state: mouse_state_t;
  signal mouse_send_state,next_mouse_send_state: mouse_send_state_t;
  signal byte_cnt,next_byte_cnt                : natural range 0 to Mouse_Init_ARRAY_c'high;
  signal set_button_stat                       : std_ulogic;
  signal next_button_stat,button_stat          : std_ulogic_vector(7 downto 0);
  signal set_delta_x                           : std_ulogic;
  signal next_delta_x,delta_x                  : std_ulogic_vector(7 downto 0);
  signal set_delta_y                           : std_ulogic;
  signal next_delta_y,delta_y                  : std_ulogic_vector(7 downto 0);
  signal next_mouse_stb, mouse_stb             : std_ulogic;
  signal MouseError                            : std_ulogic;

begin

    PS2if : PS2_Interface
      port map (
        reset_n_i   => reset_n_i,
        clk_i       => clk_i,    
        Ps2Clk_io   => Ps2Clk_io,
        Ps2Dat_io   => Ps2Dat_io,
        data_o      => DataReg(7 downto 0),
        data_stb_o  => DataReg(8),
        ack_o       => open,
        error_stb_o => MouseError,
        data_i      => next_CmdReg,
        data_stb_i  => set_CmdReg
      );
 
  
  mouse_fsm :process(mouse_state,mouse_send_state,DataReg,
                     byte_cnt,button_stat) is
    variable cmd_v : std_ulogic_vector(7 downto 0);                            
  begin
    next_mouse_state      <= mouse_state;
    next_mouse_send_state <= mouse_send_state;
--    set_decoder_enable  <= '0';
    set_CmdReg          <= '0';
    next_CmdReg         <= (others => '-');
    next_byte_cnt       <= byte_cnt;
    set_button_stat     <= '0';
    next_button_stat    <= (others => '-');
    set_delta_x         <= '0';
    next_delta_x        <= (others => '-');
    set_delta_y         <= '0';
    next_delta_y        <= (others => '-');
    next_mouse_stb      <= '0';
    decoder_enable      <= '1';
    
    case mouse_state is
      when IDLE_e =>
        next_mouse_send_state <= IDLE_e;
        if DataReg(8)='1' then
          if DataReg(7 downto 0)=PWRON_CODE_c then
            -- Mouse has finished power-up
            -- so initialize it
            next_mouse_state   <= READ_ID_e;
            next_byte_cnt      <= 0;
          else
            case byte_cnt is
              when 0 =>
                set_button_stat  <= '1';
                next_button_stat <= DataReg(7 downto 0);
                next_byte_cnt    <= byte_cnt + 1;
              when 1 =>
                set_delta_x      <= '1';
                next_delta_x     <= DataReg(7 downto 0);
                next_byte_cnt    <= byte_cnt + 1;
              when 2 =>
                set_delta_y      <= '1';
                next_delta_y     <= DataReg(7 downto 0);
                next_byte_cnt    <= 0;
                if button_stat(3) = '1' then
                  next_mouse_stb   <= '1';
                else
                  next_mouse_state   <= RESET_MOUSE_e;
                end if;
                
              when others =>
                next_byte_cnt <= 0;
            end case;
          end if;
        end if;
      when RESET_MOUSE_e =>
        decoder_enable <= '0'; -- disable decoder during init
        case mouse_send_state is
          when IDLE_e =>
            next_CmdReg           <= RESET_CODE_c;
            set_CmdReg            <= '1';
            next_mouse_send_state <= WAIT_ACK_e;
          when WAIT_ACK_e =>
            if DataReg(8)='1' and
               DataReg(7 downto 0)=ACK_CODE_c then
              next_mouse_send_state <= IDLE_e;
              next_mouse_state      <= IDLE_e;
            end if;
          when others => 
            next_mouse_send_state <= IDLE_e;
        end case;
      when READ_ID_e => 
        decoder_enable <= '0'; -- disable decoder during init
        -- wait until Mouse has sent ID
        if DataReg(8)='1' then
          if DataReg(7 downto 0)=X"00" then
            next_mouse_state <= INIT_e;
          else
            next_mouse_state <= IDLE_e;
          end if;
        end if;
        
      when INIT_e =>
        decoder_enable <= '0'; -- disable decoder during init
        case mouse_send_state is
          when IDLE_e =>
            
            cmd_v := Mouse_Init_ARRAY_c(byte_cnt);
            next_CmdReg           <= cmd_v;
            set_CmdReg            <= '1';
            next_mouse_send_state <= WAIT_ACK_e;

          when WAIT_ACK_e =>
            if DataReg(8)='1' and
               DataReg(7 downto 0)=ACK_CODE_c then
              next_mouse_send_state <= IDLE_e;
              if byte_cnt = Mouse_Init_ARRAY_c'high then
                next_mouse_state <= IDLE_e;
                next_byte_cnt    <= 0;
                -- init done
              else
                next_byte_cnt <= byte_cnt + 1;
              end if;
            end if;
          when others => 
            next_mouse_send_state <= IDLE_e;
        end case;
      when others =>
        next_mouse_send_state <= IDLE_e;
    end case;
  end process mouse_fsm;
  
  process (Clk_i, reset_n_i) is
  begin  -- process
    if reset_n_i = ResetActive_c then
      mouse_state      <= RESET_MOUSE_e;
-- pragma translate_off      
      mouse_state      <= IDLE_e;
-- pragma translate_on
      mouse_send_state <= IDLE_e;
      byte_cnt         <= 0;
      button_stat      <= (others => '0');
      delta_x          <= (others => '0');
      delta_y          <= (others => '0');
      mouse_stb        <= '0';
    elsif Clk_i'event and Clk_i='1' then
      mouse_state      <= next_mouse_state;
      mouse_send_state <= next_mouse_send_state;
      byte_cnt         <= next_byte_cnt;
      if set_button_stat = '1' then
        button_stat <= next_button_stat;
      end if;
      if set_delta_x = '1' then
        delta_x <= next_delta_x;
      end if;
      if set_delta_y = '1' then
        delta_y <= next_delta_y;
      end if;
      mouse_stb <= next_mouse_stb;
    end if;
  end process;
  
  --monitoring_o(8 downto 0)   <= DataReg;
  --monitoring_o(9)            <= decoder_enable;
  --monitoring_o(10)           <= set_CmdReg;
  --monitoring_o(13 downto 11) <= button_stat(2 downto 0);
  --monitoring_o(14)           <= mouse_stb;
--  monitoring_o(15)           <= MouseError;
  
  process(byte_cnt)
    variable tmp_v : unsigned(2 downto 0);
  begin
    tmp_v := to_unsigned(byte_cnt,3);
   -- monitoring_o(15) <= tmp_v(0);
  end process;
   
  ------------------------------------------------------------
  -- NKC Mouse Logic
  ------------------------------------------------------------
  nkc_mouse: block
    -- registers
    signal key_stat            : std_ulogic_vector(7 downto 0);
    signal up_reg,up_cnt       : unsigned(7 downto 0);
    signal down_reg,down_cnt   : unsigned(7 downto 0);
    signal right_reg,right_cnt : unsigned(7 downto 0);
    signal left_reg,left_cnt   : unsigned(7 downto 0);
--    signal mouse_fsm           : std_ulogic_vector(1 downto 0);
--    signal send_fsm            : std_ulogic;
  begin
    
--  with mouse_state select
--    mouse_fsm <=  "00" when IDLE_e,
--                  "01" when RESET_MOUSE_e,
--                  "10" when READ_ID_e,
--                  "11" when INIT_e;
--  with mouse_send_state select
--    send_fsm <=   '0' when IDLE_e,
--                  '1' when others;
    
    
    with to_integer(unsigned(Adr_i(2 downto 0))) select
      DataOut_o <=  key_stat                      when 3, -- 0x8B
                    std_ulogic_vector(down_reg)   when 4,
                    std_ulogic_vector(up_reg)     when 5,
                    std_ulogic_vector(right_reg)  when 6,
                    std_ulogic_vector(left_reg)   when 7,
--                    "000"& send_fsm & "00" &
--                    mouse_fsm                     when 0,
                    (others => '0') when others;
    
    process(button_stat,byte_cnt)
      variable tmp_v : unsigned(2 downto 0);
    begin
      tmp_v := to_unsigned(byte_cnt,3);
      key_stat             <= (others => '1');
--      key_stat(2 downto 0) <= std_ulogic_vector(tmp_v);
--      key_stat(7 downto 5) <= button_stat(2 downto 0);
      key_stat(7 downto 5) <= not(button_stat(0) & button_stat(1) & button_stat(2));
    end process;
  
    process (Clk_i, reset_n_i) is
      variable left_v,right_v,up_v,down_v : unsigned(7 downto 0);
      
      function abs_f(delta : in std_ulogic_vector) return unsigned is
        variable tmp_v : signed(delta'range);
      begin
        tmp_v := abs(signed(delta));
        return shift_right(unsigned(tmp_v),2);
      end abs_f;
    begin  -- process
      if reset_n_i = ResetActive_c then
        up_reg    <= (others => '0');
        down_reg  <= (others => '0');
        right_reg <= (others => '0');
        left_reg  <= (others => '0');
        up_cnt    <= (others => '0');
        down_cnt  <= (others => '0');
        right_cnt <= (others => '0');
        left_cnt  <= (others => '0');
        
      elsif Clk_i'event and Clk_i='1' then
        left_v  := left_cnt;
        right_v := right_cnt;
        up_v    := up_cnt;
        down_v  := down_cnt;
        
        if (en_i and Wr_i)='1' and Adr_i="110" then
          left_v  := (others => '0');
          right_v := (others => '0');
          up_v    := (others => '0');
          down_v  := (others => '0');
        end if;
        
        
        if mouse_stb = '1' then
          if button_stat(4)='1' then
            left_v  := left_v  + abs_f(delta_x);
--            left_v  := left_v  - unsigned(delta_x(7 downto 0));
          else
            right_v := right_v + abs_f(delta_x);
--            right_v := right_v + unsigned(delta_x(7 downto 0));
          end if;
          if button_stat(5)='1' then
            up_v  := up_v   + abs_f(delta_y);
--            up_v  := up_v   - unsigned(delta_y(7 downto 0));
          else
            down_v := down_v + abs_f(delta_y);
--            down_v := down_v + unsigned(delta_y(7 downto 0));
          end if;
        end if;
        left_cnt  <= left_v;
        right_cnt <= right_v;
        up_cnt    <= up_v;
        down_cnt  <= down_v;
        if (en_i and Wr_i)='1' and Adr_i="101" then --8D
          up_reg    <= up_cnt;   
          down_reg  <= down_cnt; 
          right_reg <= right_cnt;
          left_reg  <= left_cnt;
        end if;
      end if;
      
    end process;
  end block;
end rtl;
