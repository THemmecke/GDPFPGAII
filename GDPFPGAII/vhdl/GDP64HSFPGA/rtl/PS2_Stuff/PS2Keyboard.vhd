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

entity PS2Keyboard is
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
    KeyCS_i   : in  std_ulogic;
    DipCS_i   : in  std_ulogic;
    KOptCS_i  : in  std_ulogic;
    Rd_i      : in  std_ulogic;
    Wr_i	  : in  std_ulogic;
    DataIn_i  : in std_ulogic_vector(7 downto 0);
    DataOut_o : out std_ulogic_vector(7 downto 0);
	
	is68000_o : out std_ulogic; -- signal to wrapper
    --------------------------
    -- Monitoring (Debug) signals
    --------------------------
    monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
  );
end PS2Keyboard;

architecture rtl of PS2Keyboard is
  

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
  
  component PS2_Decoder
    port(
      reset_n_i    : in  std_logic;
      clk_i        : in  std_logic;
      --------------------------
      -- PS/2 Keyboard signals
      --------------------------
      enable_i         : in  std_ulogic;
      ScanCode_i       : in  std_ulogic_vector(7 downto 0);
      ScanCode_stb_i   : in  std_ulogic;
      Leds_o           : out std_ulogic_vector(2 downto  0);
      Leds_stb_o       : out std_ulogic;
      --------------------------
      -- Decoded ASCII
      --------------------------
      NkcCode_o        : out std_ulogic_vector(6 downto 0);
      NkcCode_stb_o    : out std_ulogic
    );
  end component;
 
  component ps2_fifo
    port (Data: in  std_logic_vector(7 downto 0); Clock: in  std_logic; 
        WrEn: in  std_logic; RdEn: in  std_logic; Reset: in  std_logic; 
        Q: out  std_logic_vector(7 downto 0); Empty: out  std_logic; 
        Full: out  std_logic);
  end component;
  
  constant transInit_c  : natural :=2400;    -- 60 uS Time to hold CLK low (to initiate a Transmission)
  constant transStart_c : natural :=2480;    -- 2 uS additonal Time where Data and Clockline are both LOW
--  type   Ps2Direction_t is (FromKbd, ToKbd);
--  type   state_t        is (IDLE, DELAY,START, DATA, STOP);
--  type   clkEdge_t      is (NONE, RISING, FALLING);
  type   FifoRdState_t  is (IDLE, RDPEND);
 
  type kbd_init_state_t  is (IDLE_e, INIT_e, SET_LED_e);
  type kbd_send_state_t  is (IDLE_e, WAIT_ACK_e);
  
  constant EXT_CODE_c            : std_ulogic_vector(7 downto 0) := X"E0";
  constant SETLED_CODE_c         : std_ulogic_vector(7 downto 0) := X"ED";
  constant ECHO_CODE_c           : std_ulogic_vector(7 downto 0) := X"EE";
  constant BREAK_CODE_c          : std_ulogic_vector(7 downto 0) := X"F0";
  constant SET_TYPEMATIC_RATE_c  : std_ulogic_vector(7 downto 0) := X"F3";
  constant KBD_ENABLE_c          : std_ulogic_vector(7 downto 0) := X"F4";
  
  constant TYPEMATICRATE_30cps_c : std_ulogic_vector(7 downto 0) := X"00";
  constant TYPEMATICRATE_24cps_c : std_ulogic_vector(7 downto 0) := X"02";
  constant TYPEMATICRATE_21cps_c : std_ulogic_vector(7 downto 0) := X"04";
  constant TYPEMATICRATE_16cps_c : std_ulogic_vector(7 downto 0) := X"07";
  constant TYPEMATICRATE_12cps_c : std_ulogic_vector(7 downto 0) := X"0A";
  constant TYPEMATICRATE_10cps_c : std_ulogic_vector(7 downto 0) := X"0C";
  constant TYPEMATICRATE_8cps_c  : std_ulogic_vector(7 downto 0) := X"0F";
  constant TYPEMATICRATE_6cps_c  : std_ulogic_vector(7 downto 0) := X"12";

  constant TYPEMAT_DELAY_1s_c    : std_ulogic_vector(7 downto 0) := X"60";
  constant TYPEMAT_DELAY_750ms_c : std_ulogic_vector(7 downto 0) := X"40";
  constant TYPEMAT_DELAY_500ms_c : std_ulogic_vector(7 downto 0) := X"20";
  constant TYPEMAT_DELAY_250ms_c : std_ulogic_vector(7 downto 0) := X"00";
  
  constant PWRON_CODE_c : std_ulogic_vector(7 downto 0) := X"AA";
  constant ACK_CODE_c   : std_ulogic_vector(7 downto 0) := X"FA";
  
  type Kbd_Init_ARRAY_t is array(0 to 2) of std_ulogic_vector(7 downto 0);
  constant Kbd_Init_ARRAY_c : Kbd_Init_ARRAY_t := (
    SET_TYPEMATIC_RATE_c, 
    std_ulogic_vector(unsigned(TYPEMAT_DELAY_250ms_c) + unsigned(TYPEMATICRATE_30cps_c)),
    KBD_ENABLE_c
  );
  
--  signal Ps2Direction,NextPs2Direction     : Ps2Direction_t;
--  signal state, nextState                  : state_t;
--  signal ps2ClkEdge                        : clkEdge_t;
--  signal q                                 : std_ulogic_vector(3 downto 0);
--  signal Ps2ClkOld                         : std_ulogic;
--  signal sReg                              : std_ulogic_vector(8 downto 0);
----  signal Parity                            : std_ulogic;
--  signal delayCnt                          : unsigned (11 downto 0);
--  signal Ps2Clk,Ps2ClockHtranslate1        : std_ulogic;
--  signal Ps2DataOut, Ps2ClockOut           : std_ulogic;
  signal DataReg                           : std_ulogic_vector(8 downto 0);
  signal set_CmdReg                        : std_ulogic;
  signal next_CmdReg                       : std_ulogic_vector(7 downto 0);
--  signal Status,StatusReg                  : std_ulogic_vector(6 downto 0);
--  signal ackReceived                       : std_ulogic;       
  signal Reset                             : std_ulogic;
  signal NkcCode                           : std_ulogic_vector(6 downto 0);
  signal FifoRdData                        : std_logic_vector(7 downto 0);
  signal NkcCode_stb                       : std_ulogic;
  signal FifoRdEn                          : std_ulogic;
  signal FifoEmpty                         : std_ulogic;
  signal FifoFull                          : std_ulogic;
  signal FifoRdState, next_FifoRdState     : FifoRdState_t;
  signal next_KbdDataReg_valid             : std_ulogic;
  signal KbdDataReg_valid                  : std_ulogic;
  signal Leds                              : std_ulogic_vector(2 downto 0);
  signal Leds_stb                          : std_ulogic;
  signal decoder_enable                    : std_ulogic;
  signal kbd_init_state,next_kbd_init_state: kbd_init_state_t;
  signal kbd_send_state,next_kbd_send_state: kbd_send_state_t;
  signal init_cnt,next_init_cnt            : natural range 0 to Kbd_Init_ARRAY_c'high;
  
  signal KeyOptsReg			: std_ulogic_vector(7 downto 0);							-- TH: Keyboard Options Register 
  																						-- Bit 0 = 0 => NKC Mode
  																						-- Bit 0 = 1 => ScanCode Mode
																						-- Bit 7 = 1 => Kbd Data Available
  signal FifoData				: std_ulogic_vector(7 downto 0);
  signal FifoWrEn           : std_ulogic;
         
--  Function CalcParity(Data : in std_ulogic_vector) return std_ulogic is
--    variable p: std_ukllogic:='1';
--  begin  
--    for i in Data'range loop
--      p:=p xor Data(i);
--    end loop;
--    return p;
--  end;   
begin

  Reset <= not Reset_n_i;

--  -- Needed for simulation with 'H' value instead of '1'.
--  Ps2ClockHtranslate1 <= To_X01Z(Ps2Clk_io);
--  
--  IS1 : InputSync
--   generic map (
--     levels_g => 2,
--     ResetValue_g => '1')
--   port map (
--     Input => Ps2ClockHtranslate1,
--     clk   => Clk_i,
--     clr_n => reset_n_i,
--     q     => Ps2Clk);
--
--
--  Ps2Dat_io <= '0' when Ps2DataOut = '0' else
--               'Z';
--  Ps2Clk_io <= '0' when Ps2ClockOut = '0' else
--               'Z';
--
--  ps2ClkEdge <= RISING  when (    Ps2Clk and not Ps2ClkOld)='1' else
--                FALLING when (not Ps2Clk and     Ps2ClkOld)='1' else
--                NONE;
--
--  Comb : process (state, ps2ClkEdge, q, Ps2Direction, CmdReg, delayCnt)
--  begin  -- process Comb
--    nextstate <= state;
--    NextPs2Direction <= Ps2Direction;
--    case state is
--      when IDLE =>
--        case Ps2Direction is
--          when ToKbd =>
--            nextstate <= DELAY;
--          when FromKbd =>
--            if ps2ClkEdge = RISING then
--              nextstate <= DATA;
--            elsif CmdReg(8) = '1' then
--              NextPs2Direction <= ToKbd;
--            end if;
--         end case;
--      when DELAY =>
--        if delayCnt = transInit_c then
--            nextstate <= START;
--        end if;
--      when START =>
--        if delayCnt = transStart_c then
--          nextstate <= DATA;
--        end if;
--      when DATA =>
--        case Ps2Direction is
--          when ToKbd =>
--            if q = std_ulogic_vector(to_unsigned(11, q'length)) then
--              nextstate <= STOP;
--            end if;
--          when FromKbd =>
--            if q = std_ulogic_vector(to_unsigned(9, q'length)) then
--              nextstate <= STOP;
--            end if;
--         end case;
--      when STOP =>
--        if ps2ClkEdge = RISING then
--          nextstate        <= IDLE;
--          NextPs2Direction <= FromKbd;
--        end if;
--      end case;
--  end process Comb;
--
--  -- Sequential Process
--  seq: process (Clk_i, reset_n_i) is
--  begin  -- process
--    if reset_n_i = ResetActive_c then
--      state        <= IDLE;
--      Ps2Direction <= FromKbd;
--      
--      sReg        <= (others => '0');
--      q           <= (others => '0');
--      delayCnt    <= (others => '0');
--      Ps2ClkOld   <= '1';
--      Ps2ClockOut <= '1';
--      Ps2DataOut  <= '1';
--      CmdReg      <= (others => '0');
--      DataReg     <= (others => '0');
----      StatusReg   <= (others => '0');
----      RcvOv       <= '0';
--      ackReceived <= '0';
--      
--    elsif Clk_i'event and Clk_i='1' then
--      Ps2ClkOld    <= Ps2Clk;
--      state        <= nextstate;
--      Ps2Direction <= NextPs2Direction;
--      DataReg(8)   <= '0';
--      case state is
--        when IDLE =>
--          Ps2ClockOut <= '1';
--          Ps2DataOut  <= '1';
--          q           <= (others =>'0');
--          delayCnt    <= (others =>'0');
--          case Ps2Direction is
--            when ToKbd =>
--              sReg      <= CalcParity(CmdReg(7 downto 0))& CmdReg(7 downto 0);
--              CmdReg(8) <= '0';
--            when FromKbd =>
--          end case;
--        when DELAY =>
--          Ps2ClockOut <= '0';           -- initiate Transmitt by holding clockline LOW for 60uS
--          Ps2DataOut  <= '1';
--          delayCnt    <= delayCnt + 1;
--        when START =>
--          delayCnt    <= delayCnt + 1;
--          Ps2ClockOut <= '0';
--          Ps2DataOut  <= '0';           -- Delay is over. Bring Dataline low now.
--        when DATA =>
--          Ps2ClockOut <= '1';           -- bring clock line to HIGH (switch to input mode) 
--          case Ps2Direction is
--            when ToKbd =>
--              if ps2ClkEdge = FALLING then
--                Ps2DataOut <= sReg(0);
--                sReg       <= '1' & sReg(8 downto 1);  -- shift in the stopbit
--                q<=std_ulogic_vector(unsigned(q) + 1);
--              end if;
--            when FromKbd =>
--              Ps2DataOut <= '1';
--              if ps2ClkEdge = RISING then
--                sReg <= Ps2Dat_io & sReg(8 downto 1);
--                q<=std_ulogic_vector(unsigned(q) + 1);
--              end if;
--            end case;
--        when STOP => 
--            Ps2DataOut <= '1';
--            case Ps2Direction is
--            when ToKbd =>
--              if ps2ClkEdge = RISING then    
--                ackReceived <= not Ps2Dat_io;
--              end if;
--            when FromKbd =>
--              if ps2ClkEdge = RISING then
----                if DataReg(8)='1' then
----                  RcvOv<='1';
----                end if;
--                DataReg<="0" & sReg(7 downto 0);
--                if CalcParity(sReg)='0' then
--                  DataReg(8)<='1';
--                end if;
--              end if;
--            end case;
--        when others => null;
--      end case;
--      
--      if set_CmdReg = '1' then
--        CmdReg <= "1" & next_CmdReg;
--      end if;
--    end if;
--  end process seq;



    PS2if : PS2_Interface
      port map (
        reset_n_i   => reset_n_i,
        clk_i       => clk_i,    
        Ps2Clk_io   => Ps2Clk_io,
        Ps2Dat_io   => Ps2Dat_io,
        data_o      => DataReg(7 downto 0),
        data_stb_o  => DataReg(8),
        ack_o       => open,
        error_stb_o => open,
        data_i      => next_CmdReg,
        data_stb_i  => set_CmdReg
      );

  PS2dec : PS2_Decoder
    port map(
      reset_n_i        => reset_n_i,
      clk_i            => clk_i,
      enable_i         => decoder_enable,
      ScanCode_i       => DataReg(7 downto 0),
      ScanCode_stb_i   => DataReg(8),
      Leds_o           => Leds,
      Leds_stb_o       => Leds_stb,
      NkcCode_o        => NkcCode,
      NkcCode_stb_o    => NkcCode_stb
    );
    
  PS2FiFo : ps2_fifo
    port map (
      Clock    => clk_i,
      Reset    => Reset, 
      Data     => std_logic_vector(FifoData), 
      WrEn     => FifoWrEn,
      RdEn     => FifoRdEn,
      Q        => FifoRdData, 
      Empty    => FifoEmpty, 
      Full     => FifoFull);

--      FifoRdEn    <= '0';

  FifoData <=   '0' & NkcCode 	when KeyOptsReg(0) = '0' else	-- NKC mode: write decoded value to fifo
  				DataReg(7 downto 0);						-- ScanCode Mode: write scan code to fifo
  FifoWrEn <=   NkcCode_stb 	when KeyOptsReg(0) = '0' else -- NKC mode: use NkcCode_stb as fifo WR signal
  				DataReg(8);								    -- ScanCode Mode: use DataReg bit 8  as fifo WR signal
  
  --FifoData <=   '0' & NkcCode;
  --FifoWrEn <=   NkcCode_stb;
  
  is68000_o <= KeyOptsReg(1);
  
  process(FifoRdState,KbdDataReg_valid,FifoEmpty,DipCS_i,Rd_i,KeyCS_i,KeyOptsReg)
  begin
    next_FifoRdState      <= FifoRdState;
    next_KbdDataReg_valid <= KbdDataReg_valid;
    FifoRdEn              <= '0';
    
    case FifoRdState is
      when IDLE =>
        if FifoEmpty = '0' then
          FifoRdEn              <= '1';
          next_FifoRdState      <= RDPEND;
          next_KbdDataReg_valid <= '1';
        end if;
      when RDPEND =>
        case KeyOptsReg(0) is
          when '0' =>			-- NKC Mode
        	-- wait for ACK
        	if (DipCS_i and Rd_i) = '1' then
          		next_FifoRdState      <= IDLE;
          		next_KbdDataReg_valid <= '0';
        	end if;
          when '1' =>			-- ScanCode Mode
          	if (KeyCS_i and Rd_i) = '1' then
          		next_FifoRdState      <= IDLE;
			end if;					
          when others =>		-- Other Modes
          	next_FifoRdState <= IDLE;
      	end case;    	
      when others => 
        next_FifoRdState <= IDLE;
    end case;
  end process;
  
  
  process (Clk_i, reset_n_i) is
  begin  -- process
    if reset_n_i = ResetActive_c then
      FifoRdState      <= IDLE;
      KbdDataReg_valid <= '0';
      KeyOptsReg <= (others => '0');															-- TH: reset Keyboard Options Register	
    elsif Clk_i'event and Clk_i='1' then
      FifoRdState      <= next_FifoRdState ;
      KbdDataReg_valid <= next_KbdDataReg_valid;

      KeyOptsReg(7) <= not FifoEmpty;			-- Bit 7 of Options Register signals available Keyboard Data

      if(KOptCS_i and Wr_i) = '1' then			-- write options register
	KeyOptsReg(1 downto 0) <= DataIn_i(1 downto 0);
      elsif(KOptCS_i) = '1' then				-- read options register
	DataOut_o <= KeyOptsReg;
      elsif(KeyOptsReg(0) = '0') then			-- read fifo in NKC Mode
   	DataOut_o <= not KbdDataReg_valid & std_ulogic_vector(FifoRdData(6 downto 0));
      else										-- read fifo in ScanCode Mode
	 DataOut_o <= std_ulogic_vector(FifoRdData);	 
      end if;	

    end if;
  end process;
  
  
--  DataOut_o <=  KeyOptsReg when (KOptCS_i) = '1'  else 								-- TH: read Keyboard Options Register
--  																					-- (KOptCS_i and Rd_i) funktioniert nicht, da DataOut asynchron ist !! Muss ja auch nicht
--  		not KbdDataReg_valid & std_ulogic_vector(FifoRdData(6 downto 0)) when KeyOptsReg = X"0" else
--
--  		std_ulogic_vector(FifoRdData) when FifoEmpty = '0' else
--
--		X"00";
  
  kbd_init_send_fsm :process(kbd_init_state,kbd_send_state,Leds,Leds_stb,DataReg,
                            init_cnt) is
    variable cmd_v : std_ulogic_vector(7 downto 0);                            
  begin
    next_kbd_init_state <= kbd_init_state;
    next_kbd_send_state <= kbd_send_state;
--    set_decoder_enable  <= '0';
    set_CmdReg          <= '0';
    next_CmdReg         <= (others => '-');
    next_init_cnt       <= init_cnt;
    decoder_enable      <= '1';
    
    case kbd_init_state is
      when IDLE_e =>
        next_kbd_send_state <= IDLE_e;
        next_init_cnt       <= 0;
        if DataReg(8)='1' and
           DataReg(7 downto 0)=PWRON_CODE_c then
           -- Keyboard has finished power-up
           -- so initialize it
           next_kbd_init_state <= INIT_e;
        end if;
        if Leds_stb = '1' then
          next_kbd_init_state <= SET_LED_e;
        end if;
        
      when  SET_LED_e =>
--        decoder_enable <= '0';
        case kbd_send_state is
          when IDLE_e =>
            case init_cnt is
              when 0 =>
                next_CmdReg         <= SETLED_CODE_c;
                set_CmdReg          <= '1';
                next_kbd_send_state <= WAIT_ACK_e;
              when 1 =>
                next_CmdReg         <= "00000"&Leds;
                set_CmdReg          <= '1';
                next_kbd_send_state <= WAIT_ACK_e;
              when others =>
                next_kbd_init_state <= IDLE_e;
            end case;
            
          when WAIT_ACK_e =>
            if DataReg(8)='1' and
               DataReg(7 downto 0)=ACK_CODE_c then
              next_kbd_send_state <= IDLE_e;
              next_init_cnt       <= init_cnt + 1;
            end if;
          when others => 
            next_kbd_send_state <= IDLE_e;
        end case;
        
      when INIT_e =>
        decoder_enable <= '0'; -- disable decoder during init
        case kbd_send_state is
          when IDLE_e =>
            cmd_v := Kbd_Init_ARRAY_c(init_cnt);
            next_CmdReg <= cmd_v;
            set_CmdReg  <= '1';
            next_kbd_send_state <= WAIT_ACK_e;

          when WAIT_ACK_e =>
            if DataReg(8)='1' and
               DataReg(7 downto 0)=ACK_CODE_c then
              next_kbd_send_state <= IDLE_e;
              if init_cnt = Kbd_Init_ARRAY_c'high then
                -- init done
--              next_kbd_init_state <= IDLE_e;
                next_kbd_init_state <= SET_LED_e;
                next_init_cnt       <= 0;
              else
                next_init_cnt       <= init_cnt + 1;
              end if;
            end if;
          when others => 
            next_kbd_send_state <= IDLE_e;
        end case;
      when others =>
        next_kbd_init_state <= IDLE_e;
    end case;
  end process kbd_init_send_fsm;
  
  process (Clk_i, reset_n_i) is
  begin  -- process
    if reset_n_i = ResetActive_c then
      kbd_init_state <= IDLE_e;
      kbd_send_state <= IDLE_e;
      init_cnt       <= 0;
--      decoder_enable <= '0';
    elsif Clk_i'event and Clk_i='1' then
      kbd_init_state <= next_kbd_init_state;
      kbd_send_state <= next_kbd_send_state;
      init_cnt       <= next_init_cnt;
--      if set_decoder_enable = '1' then
--        decoder_enable <= '1';
--      end if;
    end if;
  end process;
  
  monitoring_o(8 downto 0) <= DataReg;
  monitoring_o(9)          <= decoder_enable;
--  monitoring_o(10)         <= CmdReg(8);
  monitoring_o(15 downto 10) <= (others => 'Z');
end rtl;
