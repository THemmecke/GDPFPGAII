--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : Soundgenerator - Toplevel unit
-- File        : WF2149ip_top_soc.vhd
-- Description : YM2149 / AY-3-891X compatible sound generator.
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2007 by Andreas Voggeneder
--------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use work.wf2149ip_pkg.all;

entity WF2149IP_TOP_SOC is
    port(
        
        SYS_CLK     : in std_ulogic; -- Read the inforation in the header!
        RESETn      : in std_ulogic;

        WAV_CLK     : in std_ulogic; -- Read the inforation in the header!
        SELn        : in std_ulogic;
        
        BDIR        : in std_ulogic;
        BC2, BC1    : in std_ulogic;

        A9n, A8     : in std_ulogic;
        DA_IN       : in std_ulogic_vector(7 downto 0);
        DA_OUT      : out std_ulogic_vector(7 downto 0);
        DA_EN       : out std_ulogic;
        
        IO_A_IN     : in std_ulogic_vector(7 downto 0);
        IO_A_OUT    : out std_ulogic_vector(7 downto 0);
        IO_A_EN     : out std_ulogic;
        IO_B_IN     : in std_ulogic_vector(7 downto 0);
        IO_B_OUT    : out std_ulogic_vector(7 downto 0);
        IO_B_EN     : out std_ulogic;

--        OUT_A       : out std_ulogic; -- Analog (PWM) outputs.
--        OUT_B       : out std_ulogic;
--        OUT_C       : out std_ulogic
        PWM_OUT     : out std_ulogic
    );
end WF2149IP_TOP_SOC;

architecture STRUCTURE of WF2149IP_TOP_SOC is
signal BUSCYCLE     : BUSCYCLES_t;
signal DATA_OUT_I   : std_ulogic_vector(7 downto 0);
signal DATA_EN_I    : std_ulogic;
signal WAV_STRB     : std_ulogic;
signal ADR_I        : std_ulogic_vector(3 downto 0);
signal CTRL_REG     : std_ulogic_vector(7 downto 0);
signal PORT_A       : std_ulogic_vector(7 downto 0);
signal PORT_B       : std_ulogic_vector(7 downto 0);
signal bdir_bc      : std_ulogic_vector(2 downto 0);
signal DA_OUT_i     : std_ulogic_vector(7 downto 0);
begin
    P_WAVSTRB: process(RESETn, SYS_CLK)
    variable LOCK   : boolean;
    variable TMP    : std_ulogic;
    begin
        if RESETn = '0' then
            LOCK := false;
            TMP := '0';
        elsif SYS_CLK = '1' and SYS_CLK' event then
            if WAV_CLK = '1' and LOCK = false then
                LOCK := true;
                TMP := not TMP; -- Divider by 2.
                case SELn is
                    when '1'    => WAV_STRB <= '1';
                    when others => WAV_STRB <= TMP;
                end case;
            elsif WAV_CLK = '0' then
                LOCK := false;
                WAV_STRB <= '0';
            else
                WAV_STRB <= '0';
            end if;
        end if;
    end process P_WAVSTRB;      

    bdir_bc <= BDIR & BC2 & BC1;
    with bdir_bc select
        BUSCYCLE <= INACTIVE    when "000" | "010" | "101",
                    ADDRESS     when "001" | "100" | "111",
                    READ        when "011",
                    WRITE       when others;  --"110"


    ADDRESSLATCH: process(RESETn, SYS_CLK)
    -- This process is responsible to store the desired register
    -- address. The default (after reset) is channel A fine tone 
    -- adjustment.
    begin
        if RESETn = '0' then
            ADR_I <= (others => '0');
        elsif SYS_CLK = '1' and SYS_CLK' event then
            if BUSCYCLE = ADDRESS and A9n = '0' and A8 = '1' and DA_IN(7 downto 4) = x"0" then
                ADR_I <= (DA_IN(3 downto 0));
            end if;
        end if;
    end process ADDRESSLATCH;   

    P_CTRL_REG: process(RESETn, SYS_CLK)
    -- THIS is the Control register for the mixer and for the I/O ports.
    begin
        if RESETn = '0' then
            CTRL_REG <= x"00";
        elsif SYS_CLK = '1' and SYS_CLK' event then
            if BUSCYCLE = WRITE and ADR_I = x"7" then
                CTRL_REG <= (DA_IN);
            end if;
        end if;
    end process P_CTRL_REG;
    
    DIG_PORTS: process(RESETn, SYS_CLK)
    begin
        if RESETn = '0' then
            PORT_A <= x"00";
            PORT_B <= x"00";
        elsif SYS_CLK = '1' and SYS_CLK' event then
            if BUSCYCLE = WRITE and ADR_I = x"E" then
                PORT_A <= (DA_IN);
            elsif BUSCYCLE = WRITE and ADR_I = x"F" then
                PORT_B <= (DA_IN);
            end if;
        end if; 
    end process DIG_PORTS;
    -- Set port direction to input or to output:
    IO_A_EN <= '1' when CTRL_REG(6) = '1' else '0';
    IO_B_EN <= '1' when CTRL_REG(7) = '1' else '0';
    IO_A_OUT <= PORT_A;
    IO_B_OUT <= PORT_B;

    I_PSG_WAVE: WF2149IP_WAVE
        port map(
            RESETn      => RESETn,
            SYS_CLK => SYS_CLK,

            WAV_STRB    => WAV_STRB,

            ADR         => ADR_I,
            DATA_IN     => DA_IN,
            DATA_OUT    => DATA_OUT_I,
            DATA_EN     => DATA_EN_I,
            
            BUSCYCLE    => BUSCYCLE,
            CTRL_REG    => CTRL_REG(5 downto 0),
                        
--            OUT_A       => OUT_A,
--            OUT_B       => OUT_B,
--            OUT_C       => OUT_C
            OUT_SUM      => PWM_OUT
        );

    -- Read the ports and registers:
    DA_EN <=    '1' when DATA_EN_I = '1' else
                '1' when BUSCYCLE = READ and ADR_I = x"7" else
                '1' when BUSCYCLE = READ and ADR_I = x"E" else
                '1' when BUSCYCLE = READ and ADR_I = x"F" else 
                '0';
                
    DA_OUT_i <= DATA_OUT_I when DATA_EN_I = '1' else -- WAV stuff.
                IO_A_IN    when BUSCYCLE = READ and ADR_I = x"E" else
                IO_B_IN    when BUSCYCLE = READ and ADR_I = x"F" else
                CTRL_REG   when BUSCYCLE = READ and ADR_I = x"7" else 
                (others => '-');                
                
    process(SYS_CLK)
    begin
      if rising_edge(SYS_CLK) then
        if BUSCYCLE = READ then
          DA_OUT <= DA_OUT_i;
        end if;
      end if;
    end process;

end STRUCTURE;
