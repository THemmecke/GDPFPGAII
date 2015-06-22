--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : SPI Interface for FTDI Vinculum Chip
-- File        : SPI_VDIP.vhd
-- Description : SPI Interface for attaching an VDIP to NKC.
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

entity SPI_VDIP is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    --------------------------
    -- SPI-Signals
    --------------------------
    VDIP_SCK_o  : out std_ulogic;
    VDIP_CS_o   : out std_ulogic;
    VDIP_MOSI_o : out std_ulogic;
    VDIP_MISO_i : in  std_ulogic;
    ----------------------------------
    -- Data Bus
    ----------------------------------
    Adr_i     : in  std_ulogic_vector(1 downto 0);
    en_i      : in  std_ulogic;
    DataIn_i  : in  std_ulogic_vector(7 downto 0);
    Rd_i      : in  std_ulogic;
    Wr_i      : in  std_ulogic;
    DataOut_o : out std_ulogic_vector(7 downto 0)
    --------------------------
    -- Monitoring (Debug) signals
    --------------------------
--    monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
  );
end SPI_VDIP;

architecture rtl of SPI_VDIP is

  type  state_t is (IDLE_e, START_e, DATA_e, STOP_e);  

  signal state,next_state : state_t;
  signal shift_reg        : std_ulogic_vector(10 downto 0);
  signal set_shift_reg    : std_ulogic;
  signal next_shift_reg   : std_ulogic_vector(10 downto 0);
  signal next_shift_en    : std_ulogic;
--  signal shift_en         : std_ulogic;
--  -- load shift reg from data reg
--  signal load_en          : std_ulogic;
  signal set_data_in      : std_ulogic;
  signal data_in_reg      : std_ulogic_vector(7 downto 0);
  signal bit_cnt          : unsigned(2 downto 0);
  signal set_bit_cnt      : std_ulogic;
  signal next_bit_cnt     : unsigned(2 downto 0);
  signal delay_cnt        : unsigned(2 downto 0);
  signal set_delay_cnt    : std_ulogic;
  signal next_delay_cnt   : unsigned(2 downto 0);
  signal next_vdip_sck    : std_ulogic;
  signal next_vdip_mosi   : std_ulogic;
  signal next_vdip_cs     : std_ulogic;
  signal vdip_sck         : std_ulogic;
  signal vdip_mosi        : std_ulogic;
  signal vdip_cs          : std_ulogic;
  signal next_write_req   : std_ulogic;
  signal write_req        : std_ulogic;
  signal next_vdip_stat   : std_ulogic;
  signal vdip_stat        : std_ulogic;
  signal ignore_edge      : std_ulogic;
  signal next_ignore_edge : std_ulogic;

  -- bit 0-2 SPR  SPI Clock Rate divider
  -- bit 3   SPE  SPI Enable)
  signal ctrl_reg    : std_ulogic_vector(3 downto 0);
  -- bit 7 IDLE SPI IDLE. Write to data reg only allowed when IDLE=1
  -- bit 6 WCOL (Write collision, datareg written during a data transfer, cleared by reading datareg)
  -- bit 5 WR_STAT Write Status Bit as provided by VDIP
  signal status      : std_ulogic_vector(2 downto 0);
  signal wcol        : std_ulogic;
  -- bit 0   SPADR ADDR Bit
  -- bit 1   SPRE ISSUE an Read request. Bit will be cleared by hardware
  signal cmd_reg     : std_ulogic_vector(1 downto 0);

  signal DataOut,DataOut_reg : std_ulogic_vector(7 downto 0);
  signal next_ack            : std_ulogic;

begin

    with Adr_i(1 downto 0) select
      DataOut   <=  status & "0" & ctrl_reg when "00",
                    "000000" & cmd_reg      when "01",
                    data_in_reg             when others;


  fsm_comb: process(state,ctrl_reg,en_i,Wr_i,DataIn_i,Adr_i,VDIP_MISO_i,
                    bit_cnt,delay_cnt,shift_reg,vdip_sck,vdip_mosi,vdip_cs,
                    write_req,vdip_stat,ignore_edge,cmd_reg)
    variable divider_stb_v : boolean;                    
  begin
    divider_stb_v  := false;
    next_state     <= state;
    set_bit_cnt    <= '0';
    next_bit_cnt   <= (others => '-');
    set_delay_cnt  <= '0';
    next_delay_cnt <= (others => '-');
    set_shift_reg  <= '0';
    next_shift_reg <= (others => '-');
    next_shift_en  <= '1';
    next_vdip_sck  <= vdip_sck; 
    next_vdip_mosi <= vdip_mosi;
    next_vdip_cs   <= vdip_cs;  
    next_write_req <= write_req;
    next_vdip_stat <= vdip_stat;
    set_data_in    <= '0';
    next_ignore_edge <= ignore_edge;
    next_ack       <= '0';
    
    if delay_cnt/=0 or state/=IDLE_e then
      if delay_cnt /= unsigned(ctrl_reg(2 downto 0)) then
        set_delay_cnt  <= '1';
        next_delay_cnt <= delay_cnt + 1;
      else
        set_delay_cnt  <= '1';
        next_delay_cnt <= (others => '0');
        next_vdip_sck  <= not vdip_sck;
        divider_stb_v  := true;
      end if;
    end if;   
    
    case state is
      when IDLE_e =>
        next_shift_en  <= '0';
        next_vdip_cs   <= '0';
        if ctrl_reg(3)='1' then
          if (((en_i and Wr_i)='1' and Adr_i(1 downto 0)="10") or
              cmd_reg(1)='1') then

            set_shift_reg  <= '1';
            set_bit_cnt    <= '1';
            if cmd_reg(1)='0' then
              -- Write Request
              next_write_req <= '1';
              next_bit_cnt   <= to_unsigned(3,bit_cnt'length);
              next_shift_reg <= "10" & cmd_reg(0) & DataIn_i;              
            else
              -- Read Request
              next_write_req <= '0';
              next_bit_cnt   <= to_unsigned(3,bit_cnt'length);
              next_shift_reg <= "11" & cmd_reg(0) & shift_reg(7 downto 0); 
            end if;
                        
            set_delay_cnt  <= '1';
            next_delay_cnt <= (others => '0');
            next_state     <= START_e;
          end if;
        end if;

      when START_e =>
        if divider_stb_v and vdip_sck='1' then
          -- falling edge
          next_vdip_cs   <= '1';
          next_vdip_mosi <= shift_reg(10);
          set_bit_cnt    <= '1';
          set_shift_reg  <= '1';
          next_shift_reg <= shift_reg(9 downto 0) & "0";
          if bit_cnt=0 then
            next_state     <= DATA_e;
            next_bit_cnt   <= to_unsigned(7,bit_cnt'length);
            next_vdip_mosi <= '0';
          else
            next_bit_cnt <= bit_cnt - 1;
          end if;
        end if;
        
      when DATA_e =>
        if divider_stb_v then
          if (write_req and vdip_sck)='1' then
            -- falling edge
            next_vdip_mosi <= shift_reg(10);
            set_shift_reg  <= '1';
            next_shift_reg <= shift_reg(9 downto 0) & "0";
            if bit_cnt=0 then
              next_state <= STOP_e;
            else
              set_bit_cnt    <= '1';
              next_bit_cnt   <= bit_cnt - 1;
            end if;
          elsif (not write_req and not vdip_sck)='1' then
            -- rising edge
            set_shift_reg  <= '1';
            next_shift_reg <= shift_reg(9 downto 0) & VDIP_MISO_i;
            if bit_cnt=0 then
              next_state       <= STOP_e;
              next_ignore_edge <= '1';
            else
              set_bit_cnt    <= '1';
              next_bit_cnt   <= bit_cnt - 1;
            end if;
          end if;
--          if (not write_req and vdip_sck)='1' then
--            -- falling edge, but read request
--            next_vdip_mosi <= '0';
--          end if;
        end if;
      when STOP_e =>
        if divider_stb_v then
          next_ignore_edge <= '0';
          if vdip_sck='0' then
            -- rising edge
            next_vdip_stat <= VDIP_MISO_i;
            set_data_in    <= not write_req;
          elsif ignore_edge='0' then
            -- falling edge
            next_state   <= IDLE_e;
            next_vdip_cs <= '0';
            next_ack     <= '1';
          end if;
        end if;
      when others =>
        next_state <= IDLE_e;
    end case;
    if ctrl_reg(3)='0' then
      next_state <= IDLE_e; 
      next_ack   <= '1';
    end if;
  end process fsm_comb;


    process (Clk_i, reset_n_i) is
    begin  -- process
      if reset_n_i = ResetActive_c then
        state         <= IDLE_e;
        vdip_sck      <= '0';
        vdip_mosi     <= '0';
        vdip_cs       <= '0';
        bit_cnt       <= (others => '0');
        delay_cnt     <= (others => '0');
        shift_reg     <= (others => '0');
        ctrl_reg      <= (others => '0');
--        status_reg    <= (others => '0');
        DataOut_reg   <= (others => '0');
--        ss_reg        <= (others => '0');
--        spif          <= '0';
        wcol          <= '0';
        write_req     <= '0';
        vdip_stat     <= '0';
        data_in_reg   <= (others => '0');
        ignore_edge   <= '0';
--        shift_en      <= '0';
        cmd_reg       <= (others => '0');
      elsif rising_edge(Clk_i) then 
        state <= next_state;       
        if set_bit_cnt='1' then
          bit_cnt <= next_bit_cnt;
        end if;
        if set_delay_cnt='1' then
          delay_cnt <= next_delay_cnt;
        end if;
        if set_shift_reg='1' then
          shift_reg <= next_shift_reg;
        end if;
        if set_data_in='1' then
          data_in_reg <= shift_reg(data_in_reg'range);
        end if;
        vdip_sck    <= next_vdip_sck; 
        vdip_mosi   <= next_vdip_mosi;
        vdip_cs     <= next_vdip_cs;  
        write_req   <= next_write_req;
        vdip_stat   <= next_vdip_stat;
        ignore_edge <= next_ignore_edge;
--        shift_en    <= next_shift_en;
        if next_ack = '1' then
          cmd_reg(1) <= '0';
        end if;
        if (en_i and Wr_i)='1' then
          case Adr_i(1 downto 0) is
            when "00" =>
              ctrl_reg <= DataIn_i(ctrl_reg'range);
            when "01" => 
              cmd_reg <= DataIn_i(cmd_reg'range);
            when "10" =>
              if state/=IDLE_e then
                wcol <= '1';
              end if;
            when others => null;
          end case;
        end if;
        if (en_i and Rd_i)='1' then
          DataOut_reg <= DataOut;
          if Adr_i(1 downto 0)="00" then
            wcol <= '0';
          end if;
        end if;
      end if;
    end process;

    status(2)   <= not next_shift_en;
    status(1)   <= wcol;
    status(0)   <= vdip_stat;
    DataOut_o   <= DataOut_reg;
    VDIP_SCK_o  <= vdip_sck;
    VDIP_CS_o   <= vdip_cs;
    VDIP_MOSI_o <= vdip_mosi;

end rtl;
