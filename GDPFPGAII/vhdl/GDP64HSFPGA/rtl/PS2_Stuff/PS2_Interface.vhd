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

entity PS2_Interface is
  port(
    reset_n_i    : in  std_ulogic;
    clk_i        : in  std_ulogic;
    --------------------------
    -- PS/2 Keyboard signals
    --------------------------
    -- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
    -- PS/2 bus. In all other cases an input would be sufficient.
    Ps2Clk_io    : inout std_logic;
    -- PS/2 data line. Bidirectional for reading and writing data.
    Ps2Dat_io    : inout std_logic;
    ----------------------------------
    -- Data Interface
    ----------------------------------
    data_o         : out std_ulogic_vector(7 downto 0);
    data_stb_o     : out std_ulogic;
    ack_o          : out std_ulogic;
    error_stb_o    : out std_ulogic;
    data_i         : in  std_ulogic_vector(7 downto 0);
    data_stb_i     : in  std_ulogic
  );
end PS2_Interface;

architecture rtl of PS2_Interface is
  

  component InputSync
    generic(levels_g     : natural :=2;
            ResetValue_g : std_ulogic := '0');
    port (
      Input : in  std_ulogic;
      clk   : in  std_ulogic;
      clr_n : in  std_ulogic;
      q     : out std_ulogic);
  end component;
  
  constant transInit_c  : natural :=2400;    -- 60 uS Time to hold CLK low (to initiate a Transmission)
  constant transStart_c : natural :=2480;    -- 2 uS additonal Time where Data and Clockline are both LOW
  type   Ps2Direction_t is (FromDevice, ToDevice);
  type   state_t        is (IDLE, DELAY,START, DATA, STOP);
  type   clkEdge_t      is (NONE, RISING, FALLING);
   
  signal Ps2Direction,NextPs2Direction     : Ps2Direction_t;
  signal state, nextState                  : state_t;
  signal ps2ClkEdge                        : clkEdge_t;
  signal q                                 : std_ulogic_vector(3 downto 0);
  signal Ps2ClkOld                         : std_ulogic;
  signal sReg                              : std_ulogic_vector(8 downto 0);
--  signal Parity                            : std_ulogic;
  signal delayCnt                          : unsigned (11 downto 0);
  signal Ps2Clk,Ps2ClockHtranslate1        : std_ulogic;
  signal Ps2DataOut, Ps2ClockOut           : std_ulogic;
  signal CmdReg,DataReg                    : std_ulogic_vector(8 downto 0);
--  signal set_CmdReg                        : std_ulogic;
--  signal next_CmdReg                       : std_ulogic_vector(7 downto 0);
  signal ParityError                       : std_ulogic;
  signal ackReceived                       : std_ulogic;

         
  Function CalcParity(Data : in std_ulogic_vector) return std_ulogic is
    variable p: std_ulogic:='1';
  begin  
    for i in Data'range loop
      p:=p xor Data(i);
    end loop;
    return p;
  end;   
begin
   
  -- Needed for simulation with 'H' value instead of '1'.
  Ps2ClockHtranslate1 <= To_X01Z(Ps2Clk_io);
  
  IS1 : InputSync
   generic map (
     levels_g => 2,
     ResetValue_g => '1')
   port map (
     Input => Ps2ClockHtranslate1,
     clk   => Clk_i,
     clr_n => reset_n_i,
     q     => Ps2Clk);


  Ps2Dat_io <= '0' when Ps2DataOut = '0' else
               'Z';
  Ps2Clk_io <= '0' when Ps2ClockOut = '0' else
               'Z';

  ps2ClkEdge <= RISING  when (    Ps2Clk and not Ps2ClkOld)='1' else
                FALLING when (not Ps2Clk and     Ps2ClkOld)='1' else
                NONE;

  Comb : process (state, ps2ClkEdge, q, Ps2Direction, CmdReg, delayCnt)
  begin  -- process Comb
    nextstate        <= state;
    NextPs2Direction <= Ps2Direction;
    case state is
      when IDLE =>
        case Ps2Direction is
          when ToDevice =>
            nextstate <= DELAY;
          when FromDevice =>
            if ps2ClkEdge = RISING then
              nextstate <= DATA;
            elsif CmdReg(8) = '1' then
              NextPs2Direction <= ToDevice;
            end if;
         end case;
      when DELAY =>
        if delayCnt = transInit_c then
          nextstate <= START;
        end if;
      when START =>
        if delayCnt = transStart_c then
          nextstate <= DATA;
        end if;
      when DATA =>
        case Ps2Direction is
          when ToDevice =>
            if q = std_ulogic_vector(to_unsigned(11, q'length)) then
              nextstate <= STOP;
            end if;
          when FromDevice =>
            if q = std_ulogic_vector(to_unsigned(9, q'length)) then
              nextstate <= STOP;
            end if;
         end case;
      when STOP =>
        if ps2ClkEdge = RISING then
          nextstate        <= IDLE;
          NextPs2Direction <= FromDevice;
        end if;
      end case;
  end process Comb;

  -- Sequential Process
  seq: process (Clk_i, reset_n_i) is
  begin  -- process
    if reset_n_i = ResetActive_c then
      state        <= IDLE;
      Ps2Direction <= FromDevice;
      
      sReg        <= (others => '0');
      q           <= (others => '0');
      delayCnt    <= (others => '0');
      Ps2ClkOld   <= '1';
      Ps2ClockOut <= '1';
      Ps2DataOut  <= '1';
      CmdReg      <= (others => '0');
      DataReg     <= (others => '0');
--      RcvOv       <= '0';
      ackReceived  <= '0';
      ParityError  <= '0';
    elsif Clk_i'event and Clk_i='1' then
      Ps2ClkOld    <= Ps2Clk;
      state        <= nextstate;
      Ps2Direction <= NextPs2Direction;
      DataReg(8)   <= '0';
      ParityError  <= '0';
      ackReceived  <= '0';
      case state is
        when IDLE =>
          Ps2ClockOut <= '1';
          Ps2DataOut  <= '1';
          q           <= (others =>'0');
          delayCnt    <= (others =>'0');
          case Ps2Direction is
            when ToDevice =>
              sReg      <= CalcParity(CmdReg(7 downto 0))& CmdReg(7 downto 0);
              CmdReg(8) <= '0';
            when FromDevice =>
          end case;
        when DELAY =>
          Ps2ClockOut <= '0';           -- initiate Transmitt by holding clockline LOW for 60uS
          Ps2DataOut  <= '1';
          delayCnt    <= delayCnt + 1;
        when START =>
          delayCnt    <= delayCnt + 1;
          Ps2ClockOut <= '0';
          Ps2DataOut  <= '0';           -- Delay is over. Bring Dataline low now.
        when DATA =>
          Ps2ClockOut <= '1';           -- bring clock line to HIGH (switch to input mode) 
          case Ps2Direction is
            when ToDevice =>
              if ps2ClkEdge = FALLING then
                Ps2DataOut <= sReg(0);
                sReg       <= '1' & sReg(8 downto 1);  -- shift in the stopbit
                q<=std_ulogic_vector(unsigned(q) + 1);
              end if;
            when FromDevice =>
              Ps2DataOut <= '1';
              if ps2ClkEdge = RISING then
                sReg <= Ps2Dat_io & sReg(8 downto 1);
                q<=std_ulogic_vector(unsigned(q) + 1);
              end if;
            end case;
        when STOP => 
            Ps2DataOut <= '1';
            case Ps2Direction is
            when ToDevice =>
              if ps2ClkEdge = RISING then    
                ackReceived <= not Ps2Dat_io;
              end if;
            when FromDevice =>
              if ps2ClkEdge = RISING then
--                if DataReg(8)='1' then
--                  RcvOv<='1';
--                end if;
                DataReg<="0" & sReg(7 downto 0);
                if CalcParity(sReg)='0' then
                  DataReg(8)  <= '1';
                else
                  ParityError <= '1';
                end if;
              end if;
            end case;
        when others => null;
      end case;
      
      if data_stb_i = '1' then
        CmdReg <= "1" & data_i;
      end if;
    end if;
  end process seq;

  data_o      <= DataReg(7 downto 0);
  data_stb_o  <= DataReg(8);
  error_stb_o <= ParityError;
  ack_o       <= ackReceived;
end rtl;
