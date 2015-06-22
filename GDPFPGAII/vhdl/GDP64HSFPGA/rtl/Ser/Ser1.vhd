--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : SER (6551) UART
-- File        : Ser1.vhd
-- Description : Serial Interface for NKC.
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

entity Ser1 is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    --------------------------
    -- UART signals
    --------------------------
    RxD_i    : in  std_ulogic;
    TxD_o    : out std_ulogic;
    RTS_o    : out std_ulogic;
    CTS_i    : in  std_ulogic;
    DTR_o    : out std_ulogic;
    ----------------------------------
    -- Data Bus
    ----------------------------------
    Adr_i     : in  std_ulogic_vector(1 downto 0);
    en_i      : in  std_ulogic;
    DataIn_i  : in  std_ulogic_vector(7 downto 0);
    Rd_i      : in  std_ulogic;
    Wr_i      : in  std_ulogic;
    DataOut_o : out std_ulogic_vector(7 downto 0);
    Intr_o    : out std_ulogic;
    --------------------------
    -- Monitoring (Debug) signals
    --------------------------
    monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
  );
end Ser1;

architecture rtl of Ser1 is
  
  constant CLK_FREQ_c : natural := 40e6;
  type Baud_Rate_ARRAY_t is array(0 to 15) of natural;
  constant Baud_Rate_ARRAY_c : Baud_Rate_ARRAY_t := (
    3  => CLK_FREQ_c / (115200*16),
    4  => CLK_FREQ_c / (57600*16), 
    5  => CLK_FREQ_c / (38400*16),
    6  => CLK_FREQ_c / (300*16),
    7  => CLK_FREQ_c / (600*16),
    8  => CLK_FREQ_c / (1200*16),
    9  => CLK_FREQ_c / (1800*16),
    10 => CLK_FREQ_c / (2400*16),
    11 => CLK_FREQ_c / (3600*16),
    12 => CLK_FREQ_c / (4800*16),
    13 => CLK_FREQ_c / (7200*16),
    14 => CLK_FREQ_c / (9600*16),
--    15 => CLK_FREQ_c / (19200*16),
    others => CLK_FREQ_c / (19200*16)
  );
  constant baud_cnt_bits_c : natural := 14;
  type tx_state_t is (IDLE_e, SHIFTING_e, STOP_BIT_e);
  signal Status               : std_ulogic_vector(7 downto 0);
  -- Registers 
  signal Data_in_reg          : std_ulogic_vector(8 downto 0);
  signal Data_out_reg         : std_ulogic_vector(8 downto 0);
  signal Status_reg           : std_ulogic_vector(7 downto 0);
  signal Control_reg          : std_ulogic_vector(7 downto 0);
  signal Command_reg          : std_ulogic_vector(7 downto 0);

  signal baud_cnt             : unsigned(baud_cnt_bits_c -1 downto 0);
  signal baud_stb             : std_ulogic;


  signal rxd_sync             : std_ulogic;
  signal CTS_sync             : std_ulogic;

  signal soft_reset           : std_ulogic;
  signal max_bit_cnt          : natural range 5 to 8; -- Wordlength
  -- Transmitter signals
  signal tx_state             : tx_state_t;
  signal next_tx_state        : tx_state_t;
  signal tx_ack               : std_ulogic;
  signal TxD,next_TxD         : std_ulogic;
  signal tx_sr                : std_ulogic_vector(8 downto 0);
  signal set_tx_sr            : std_ulogic;
  signal next_tx_sr           : std_ulogic_vector(8 downto 0);
  signal set_tx_baud_divider  : std_ulogic;
  signal tx_baud_divider      : unsigned(3 downto 0);
  signal next_tx_baud_divider : unsigned(3 downto 0);
  signal set_tx_bit_cnt       : std_ulogic;
  signal tx_bit_cnt           : unsigned(3 downto 0);
  signal next_tx_bit_cnt      : unsigned(3 downto 0);
  -- Receiverr signals
  type rx_state_t is (IDLE_e, SHIFTING_e, PARITY_e, STOP_BIT_e);
  signal rx_state             : rx_state_t;
  signal next_rx_state        : rx_state_t;
  signal rx_stb               : std_ulogic;
  signal set_parity_error     : std_ulogic;
  signal set_framing_error    : std_ulogic;
  signal rx_sr                : std_ulogic_vector(8 downto 0); --1start+8d
  signal set_rx_sr            : std_ulogic;
  signal next_rx_sr           : std_ulogic_vector(8 downto 0);
  signal set_rx_baud_divider  : std_ulogic;
  signal rx_baud_divider      : unsigned(3 downto 0);
  signal next_rx_baud_divider : unsigned(3 downto 0);
  signal set_rx_bit_cnt       : std_ulogic;
  signal rx_bit_cnt           : unsigned(3 downto 0);
  signal next_rx_bit_cnt      : unsigned(3 downto 0);
  signal rx_error             : std_ulogic;
  signal p_err                : std_ulogic;
  signal f_err                : std_ulogic;
  signal ov_err               : std_ulogic;
  signal set_tx_irq           : std_ulogic;
  signal tx_irq               : std_ulogic;
--  signal rdr_full             : std_ulogic;

  component InputSync
    generic(levels_g     : natural :=2;
            ResetValue_g : std_ulogic := '0');
    port (
      Input : in  std_ulogic;
      clk   : in  std_ulogic;
      clr_n : in  std_ulogic;
      q     : out std_ulogic);
  end component;

begin
  ISRxD : InputSync
   generic map (
     levels_g => 2,
     ResetValue_g => '1')
   port map (
     Input => RxD_i,
     clk   => Clk_i,
     clr_n => reset_n_i,
     q     => rxd_sync);
     
  ISCTS : InputSync
   generic map (
     levels_g => 2,
     ResetValue_g => '0')
   port map (
     Input => CTS_i,
     clk   => Clk_i,
     clr_n => reset_n_i,
     q     => cts_sync);
     
    
    Status(0) <= p_err;
    Status(1) <= f_err;
    Status(2) <= ov_err;
    Status(3) <= Data_in_reg(8);
    Status(4) <= not Data_out_reg(8);
    Status(5) <= '0'; --dcd_sync;
    Status(6) <= cts_sync; -- dsr;
    Status(7) <= '0';
    
    DTR_o <= not Command_reg(0);
    RTS_o <= '1' when Command_reg(3 downto 2)="00" else
             '0';
    TxD_o <= TxD;
    
    Intr_o <= (Data_in_reg(8) and not Command_reg(2)) or
              tx_irq;
              
              
    
    with to_integer(unsigned(Adr_i(1 downto 0))) select
      DataOut_o <=  Data_in_reg(7 downto 0) when 0,
                    Status_reg              when 1,
                    Command_reg             when 2,
                    Control_reg             when others;

    soft_reset <= en_i and Wr_i when Adr_i(1 downto 0)="01" else
                  '0';
    with to_integer(unsigned(Control_reg(6 downto 5))) select
      max_bit_cnt <= 8 when 0,
                     7 when 1,
                     6 when 2,
                     5 when others;

    process (Clk_i, reset_n_i) is
    begin  -- process
      if reset_n_i = ResetActive_c then
        Command_reg        <= (others => '0');
        Control_reg        <= (others => '0');
        Data_out_reg       <= (others => '0');
        Data_in_reg        <= (others => '0');
        Status_reg         <= (others => '0');
        p_err              <= '0';
        f_err              <= '0';
        ov_err             <= '0';
        rx_error           <= '0';
      elsif rising_edge(Clk_i) then
        if soft_reset = '1' then
          Control_reg(4 downto 0) <= (others => '0');
          Data_out_reg(8)         <= '0'; 
          Data_in_reg(8)          <= '0'; 
          p_err                   <= '0';
          f_err                   <= '0';
          ov_err                  <= '0';
        end if;
        if rx_state = IDLE_e then
          rx_error <= '0';
        end if;
        if (en_i and Rd_i)='1' and Adr_i(1 downto 0)="01" then
          Status_reg         <= Status; -- hold status stable during whole read access
        end if;
        if (Data_in_reg(8) and en_i and Rd_i)='1' and Adr_i(1 downto 0)="00" then
          Data_in_reg(8) <= '0';
        end if;
        if rx_stb = '1' then
          if Data_in_reg(8) = '1' then
            ov_err    <= '1';
            rx_error  <= '1';
          elsif rx_error='0' then
            -- Error flags are cleared after read of RDR and next error-free receipt of data
            p_err  <= '0';
            f_err  <= '0';
            ov_err <= '0';
          end if;
          if Command_reg(4)='1' then
            -- Echo-Mode
            Data_out_reg <= '1' & rx_sr(8 downto 1);
          else
            Data_in_reg <= '1' & rx_sr(8 downto 1);
          end if;
        end if;
        if set_parity_error = '1' then
          p_err     <= '1';
          rx_error  <= '1';
        end if;
        if set_framing_error = '1' then
          f_err     <= '1';
          rx_error  <= '1';
        end if;
        
        if (en_i and Wr_i)='1' then
          case Adr_i(1 downto 0) is
            when "00" => 
              if Command_reg(4)='0' then
                Data_out_reg <= '1' & DataIn_i;
              end if;
            when "10" => 
              Command_reg  <= DataIn_i;
            when "11" => 
              Control_reg  <= DataIn_i;
            when others => null;
          end case;
        end if;

        

        if tx_ack='1' then
          Data_out_reg(8) <= '0';
        end if;
      end if;
    end process;

    baudrate_gen : process(Clk_i, reset_n_i)
    begin
      if reset_n_i= ResetActive_c then
        baud_cnt <= (others => '0');
        baud_stb <= '0';
      elsif rising_edge(clk_i) then
        baud_stb <= '0';
        if baud_cnt /=0 then
          baud_cnt <= baud_cnt - 1;
        else
          baud_cnt <= to_unsigned(Baud_Rate_ARRAY_c(to_integer(unsigned(Control_reg(3 downto 0))))-1,baud_cnt_bits_c);
          baud_stb <= '1';
        end if;
      end if;
    end process;

  tx_fsm: process(tx_state, TxD, baud_stb, Data_out_reg, tx_baud_divider, tx_bit_cnt, max_bit_cnt,
          Command_reg, Control_reg, tx_sr, soft_reset)
    variable max_bit_cnt_v : natural range 5 to 10;
    variable data_v        : std_ulogic_vector(7 downto 0);
    variable tmp_v         : unsigned(tx_baud_divider'range);
    function calc_parity_f(data : in std_ulogic_vector;bits : in natural; p : in std_ulogic_vector(1 downto 0)) return std_ulogic is
      variable ret_v : std_ulogic;
    begin
      if p(1)='0' then
        ret_v := data(0);
        for i in 1 to data'high loop --bits-1 loop
          if i<=(bits-1) then
            ret_v := ret_v xor data(i);
          end if;
        end loop;
        if p(0)='0' then -- odd parity ?
          ret_v := not ret_v;
        end if;
      else
        ret_v:=not p(0);
      end if;
      return ret_v;      
    end calc_parity_f;
  begin
    next_tx_state        <= tx_state;
    next_TxD             <= TxD;
    tx_ack               <= '0';
    set_tx_baud_divider  <= '0';
    next_tx_baud_divider <= (others => '-');
    set_tx_bit_cnt       <= '0';
    next_tx_bit_cnt      <= (others => '-');
    set_tx_sr            <= '0';
    next_tx_sr           <= (others => '-');
    set_tx_irq           <= '0';

    max_bit_cnt_v := max_bit_cnt+1;
    if (Control_reg(7) or Command_reg(5)) = '1' then
      max_bit_cnt_v := max_bit_cnt_v + 1; -- add either parity or 2nd stopbit
    end if;
    
    
--    stop_bits_v := 0;
--    if Control_reg(7)='1' then
--      stop_bits_v := 1; -- 1
--      if Control_reg(6 downto 5)="11" and Command_reg(5) ='0' then
--        stop_bits_v := 2; -- 1.5
--      elsif Control_reg(6 downto 5)="00" and Command_reg(5) = '1' then
--        stop_bits_v := 0; -- 2
--      end if;
--    end if;
    
    case tx_state is
      when IDLE_e =>
        next_TxD <= '1';
--        if Command_reg(3 downto 2)="01" and
        if (baud_stb and Data_out_reg(8))='1' then
          next_tx_state        <= SHIFTING_e;
          tx_ack               <= '1';
          if Command_reg(4 downto 2)="001" then
            set_tx_irq   <= '1';
          end if;
          set_tx_baud_divider  <= '1';
          next_tx_baud_divider <= (others => '0');
          next_TxD             <= '0';
          set_tx_bit_cnt       <= '1';
          next_tx_bit_cnt      <= (others => '0');
          set_tx_sr            <= '1';
          data_v               := Data_out_reg(7 downto 0);
          case max_bit_cnt is
            when 7 =>
              data_v(7) := '1';
            when 6 =>
              data_v(7 downto 6) := (others =>'1');
            when 5 =>
              data_v(7 downto 5) := (others =>'1');
            when others => null;
          end case;
          next_tx_sr <= "1" & data_v;
          if Command_reg(5) = '1' then
            next_tx_sr(8) <= calc_parity_f(data_v,max_bit_cnt,Command_reg(7 downto 6));
          end if;
        elsif Command_reg(3 downto 2)="11" then
          next_TxD <= '0'; -- Transmitt break;
        end if;
        
      when SHIFTING_e =>
        tmp_v := (others =>'1');
        if tx_bit_cnt = max_bit_cnt_v then
          tmp_v(0) := '0';
        end if;
        if baud_stb = '1' then
          set_tx_baud_divider  <= '1';
          next_tx_baud_divider <= tx_baud_divider +1;          
          if tx_baud_divider = tmp_v then
            set_tx_bit_cnt   <= '1';
            next_tx_bit_cnt  <= tx_bit_cnt +1;
            next_TxD         <= tx_sr(0);
            set_tx_sr        <= '1';
            next_tx_sr       <= '1' & tx_sr(8 downto 1);
            if tx_bit_cnt = max_bit_cnt_v then
              if Control_reg(7 downto 5)="111" and Command_reg(5) ='0' then
                -- 1.5 stopbits
                next_tx_state <= STOP_BIT_e;
              else
                next_tx_state <= IDLE_e;
--                if Command_reg(4 downto 2)="001" then
--                  set_tx_irq   <= '1';
--                end if;
                
              end if;
            end if;
          end if;
        end if;
      when STOP_BIT_e =>
        -- only needed for 1.5 stopbits wo make 0.5 stopbits
        if baud_stb = '1' then
          set_tx_baud_divider  <= '1';
          next_tx_baud_divider <= tx_baud_divider +1;
          if tx_baud_divider="0111" then
            next_tx_state        <= IDLE_e;
            next_tx_baud_divider <= (others => '0');
--            if Command_reg(4 downto 2)="001" then
--              set_tx_irq   <= '1';
--            end if;
          end if;
        end if;
      when others => 
        next_tx_state <= IDLE_e;
    end case;
    if soft_reset='1' then
      next_tx_state <= IDLE_e;
    end if;
  end process;


  uart_tx : process(Clk_i, reset_n_i)
    begin
      if reset_n_i= ResetActive_c then
        tx_state        <= IDLE_e;
        tx_bit_cnt      <= (others => '0');
        tx_baud_divider <= (others => '0');
        TxD             <= '1';
        tx_bit_cnt      <= (others => '0');
        tx_sr           <= (others => '0');
        tx_irq          <= '0';
      elsif rising_edge(clk_i) then
        tx_state <= next_tx_state;
        TxD      <= next_TxD;
        if set_tx_baud_divider='1' then
          tx_baud_divider <= next_tx_baud_divider;
        end if;
        if set_tx_bit_cnt='1' then
          tx_bit_cnt <= next_tx_bit_cnt;
        end if;
        if set_tx_sr = '1' then
          tx_sr <= next_tx_sr;
        end if;
        if set_tx_irq ='1' then
          tx_irq <= '1';
        elsif (en_i and Wr_i)='1' and Adr_i(1 downto 0)="00" then
          tx_irq <= '0';
        end if;
      end if;
    end process;

  rx_fsm: process(rx_state,rxd_sync, baud_stb, rx_baud_divider, rx_bit_cnt, max_bit_cnt,
                  Command_reg, rx_sr, soft_reset)
--    variable max_bit_cnt_v : natural range 8 to 11;
    variable tmp_v : std_ulogic;
    function calc_parity_f(data : in std_ulogic_vector;bits : in natural; p : in std_ulogic_vector(1 downto 0)) return std_ulogic is
      variable ret_v : std_ulogic;
    begin
      if p(1)='0' then
        ret_v := data(data'low);
        for i in data'low+1 to data'high loop
          ret_v := ret_v xor data(i);
        end loop;
        if p(0)='0' then -- odd parity ?
          ret_v := not ret_v;
        end if;
      else
        ret_v:=not p(0);
      end if;
      return ret_v;      
    end calc_parity_f;
  begin
    next_rx_state        <= rx_state;
--    next_TxD             <= TxD;
    rx_stb               <= '0';
    set_rx_baud_divider  <= '0';
    next_rx_baud_divider <= (others => '-');
    set_rx_bit_cnt       <= '0';
    next_rx_bit_cnt      <= (others => '-');
    set_rx_sr            <= '0';
    next_rx_sr           <= (others => '-');
    set_parity_error     <= '0';
    set_framing_error    <= '0';

--    max_bit_cnt_v := max_bit_cnt+1; -- add startbit
--    if (Command_reg(5)) = '1' then
--      max_bit_cnt_v := max_bit_cnt_v + 1; -- add parity
--    end if;
    
    case rx_state is
      when IDLE_e =>
        if (baud_stb and not rxd_sync)='1' then
          next_rx_state        <= SHIFTING_e;
          -- set sampling point to middle of bit
          set_rx_baud_divider  <= '1';
          next_rx_baud_divider <= "1000";  -- bittime / 2
          set_rx_bit_cnt       <= '1';
          next_rx_bit_cnt      <= (others => '0');
        end if;
      when SHIFTING_e =>
        if baud_stb = '1' then
          set_rx_baud_divider  <= '1';
          next_rx_baud_divider <= rx_baud_divider + 1;          
          if rx_baud_divider = "1111" then
            set_rx_bit_cnt   <= '1';
            next_rx_bit_cnt  <= rx_bit_cnt +1;
            set_rx_sr        <= '1';
--            next_rx_sr       <= rxd_sync & rx_sr(9 downto 0);
--            next_rx_sr(max_bit_cnt downto 0) <= 
--                 rxd_sync & rx_sr(max_bit_cnt-1 downto 0); 
            case max_bit_cnt is
              when 8 =>
                next_rx_sr <= rxd_sync & rx_sr(8 downto 1);
              when 7 =>
                next_rx_sr <= "0" & rxd_sync & rx_sr(7 downto 1);
              when 6 =>
                next_rx_sr <= "00" & rxd_sync & rx_sr(6 downto 1);
              when 5 =>
                next_rx_sr <= "000" & rxd_sync & rx_sr(5 downto 1);
              when others => null;
            end case;
            if rx_bit_cnt = max_bit_cnt then
              if (Command_reg(5)) = '1' then
                next_rx_state <= PARITY_e;
              else
                next_rx_state <= STOP_BIT_e;
              end if;
            end if;
          end if;
        end if;
      when PARITY_e =>
        if baud_stb = '1' then
          set_rx_baud_divider  <= '1';
          next_rx_baud_divider <= rx_baud_divider + 1;          
          if rx_baud_divider = "1111" then
            next_rx_state <= STOP_BIT_e;
            case Command_reg(7 downto 6) is
              when "00" | "01" => -- ODD or EVEN
                if rxd_sync /= calc_parity_f(rx_sr(8 downto 1),max_bit_cnt,Command_reg(7 downto 6)) then
                  set_parity_error <= '1';
                end if;
              when "10" | "11" => -- MARK or SPACE => no checking
              when others => null;
            end case;
          end if;
        end if;
          
      when STOP_BIT_e =>
        if baud_stb = '1' then
          set_rx_baud_divider  <= '1';
          next_rx_baud_divider <= rx_baud_divider + 1;          
          if rx_baud_divider = "1111" then
            next_rx_state <= IDLE_e;
            rx_stb <= '1';
            -- check frame
            if rx_sr(0)='1' or rxd_sync='0' then
              set_framing_error <= '1';
            end if;
          end if;
        end if;
      when others => 
        next_rx_state <= IDLE_e;
    end case;
    if soft_reset='1' then
      next_rx_state <= IDLE_e;
    end if;
  end process;

  uart_rx : process(Clk_i, reset_n_i)
    begin
      if reset_n_i= ResetActive_c then
        rx_state        <= IDLE_e;
        rx_bit_cnt      <= (others => '0');
        rx_baud_divider <= (others => '0');

        rx_bit_cnt      <= (others => '0');
        rx_sr           <= (others => '0');
      elsif rising_edge(clk_i) then
        rx_state <= next_rx_state;

        if set_rx_baud_divider='1' then
          rx_baud_divider <= next_rx_baud_divider;
        end if;
        if set_rx_bit_cnt='1' then
          rx_bit_cnt <= next_rx_bit_cnt;
        end if;
        if set_rx_sr = '1' then
          rx_sr <= next_rx_sr;
        end if;
      end if;
    end process;

  monitoring_o <= (others => '0');

end rtl;
