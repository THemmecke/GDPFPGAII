--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : 
-- File        : Timer.vhd
-- Description : 16 bit General Purpose Timer for NKC.
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2008 by Andreas Voggeneder
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.DffGlobal.all;
use work.gdp_global.all;

entity Timer is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    --------------------------
    -- Interrupt
    --------------------------
    Irq_o     : out std_ulogic;
    ----------------------------------
    -- Data Bus
    ----------------------------------
    Adr_i     : in  std_ulogic_vector(1 downto 0);
    en_i      : in  std_ulogic;
    DataIn_i  : in  std_ulogic_vector(7 downto 0);
    Rd_i      : in  std_ulogic;
    Wr_i      : in  std_ulogic;
    DataOut_o : out std_ulogic_vector(7 downto 0)
  );
end Timer;

architecture rtl of Timer is
 
  -- bit 0   Run (Timer run)
  -- bit 1,2 WRM (Timer Register Write Mode) 
  --         0=Write only reload register
  --         1=Write only timer register
  --         2=Write both
  -- bit 6   TOVF Timer Overflow. Have to be cleared by software
  -- bit 7   IE (Interrupt enable)
  signal ctrl_reg     : std_ulogic_vector(7 downto 0);
  -- Status Register
  -- bit 0   OV Timer Overflow (cleared by read)
--  signal status_reg   : std_ulogic_vector(7 downto 0);
  signal tovf         : std_ulogic;
  
  signal Timer_reg    : std_ulogic_vector(15 downto 0);
  signal temp_reg     : std_ulogic_vector(7 downto 0);
  signal Reload_reg   : std_ulogic_vector(15 downto 0);
  signal prescaler    : natural range 0 to 39;
  signal DataOut      : std_ulogic_vector(7 downto 0);
  signal DataOut_reg  : std_ulogic_vector(7 downto 0);
begin
    
    with to_integer(unsigned(Adr_i(1 downto 0))) select
      DataOut   <=  ctrl_reg(7 downto 6) & "000" & 
                    ctrl_reg(2 downto 0)            when 0,
                    Timer_reg(15 downto 8)          when 1,
                    Timer_reg(7 downto 0)           when 2,
                    (others => '-')                 when others;
                    

--    status_reg <= "0000000" & tovf;   
    tovf <= ctrl_reg(6);
    
    process (Clk_i, reset_n_i) is
    begin  -- process
      if reset_n_i = ResetActive_c then
        prescaler   <= 0;
        Timer_reg   <= (others => '0');
        Reload_reg  <= (others => '0');
        temp_reg    <= (others => '0');
        ctrl_reg    <= (others => '0');
--        tovf        <= '0';
        DataOut_reg <= (others => '0');
      elsif rising_edge(Clk_i) then
        if ctrl_reg(0)='1' then
          if prescaler/=39 then
            prescaler <= prescaler +1;
          else
            prescaler <= 0;
            Timer_reg <= std_ulogic_vector(unsigned(Timer_reg)-1);
            if unsigned(Timer_reg)=0 then
              -- Timer underrun occured
--              tovf <= '1';
              ctrl_reg(6) <= '1';
              Timer_reg   <= Reload_reg;
            end if;
          end if;
        end if;
      
        if (en_i and Wr_i)='1' then
            case to_integer(unsigned(Adr_i(1 downto 0))) is
              when 0 =>
                ctrl_reg <= DataIn_i;
              when 1 => 
                temp_reg <= DataIn_i;
              when 2 =>
                if ctrl_reg(2 downto 1)="00" or
                   ctrl_reg(2 downto 1)="10" then
                  Reload_reg <= temp_reg & DataIn_i;
                end if;
                if ctrl_reg(2 downto 1)="01" or
                   ctrl_reg(2 downto 1)="10" then
                  Timer_reg <= temp_reg & DataIn_i;
                  prescaler <= 0;
                end if;
              when others => null;
            end case;
          end if;
        if (en_i and Rd_i)='1' then
          DataOut_reg <= DataOut;
--          if unsigned(Adr_i)=0 then
--            tovf <= '0';
--          end if;
        end if;
      end if;
    end process;    
   
    Irq_o     <= tovf and ctrl_reg(7);
    DataOut_o <= DataOut_reg;
    
end rtl;
