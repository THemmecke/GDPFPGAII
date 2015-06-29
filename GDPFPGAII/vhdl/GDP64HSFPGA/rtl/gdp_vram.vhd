--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : GDP 936X Display processor - VRAM-interface
-- File        : GDP_vram.vhd
-- Description :
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2007 by Andreas Voggeneder
--------------------------------------------------------------------------------

-- evtl. wr_data erweitern auf kernel_wr_data und host_wr_data ...da diese unabhängig voneinander gespeichert werden müssen/können (wg. pending)

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;
  use work.DffGlobal.all;
  use work.gdp_global.all;

entity gdp_vram is
  generic(INT_CHR_ROM_g : boolean := true);
  port (
    clk_i             : in  std_ulogic;
    clk_en_i          : in  std_ulogic;
    reset_n_i         : in  std_ulogic;
	blank_i			  : in  std_ulogic;
    -- host port (read & write 16bit)
    host_req_i        : in std_ulogic;
    host_wr_i         : in std_ulogic;
    host_addr_i       : in std_ulogic_vector(18 downto 0);
    host_data_o       : out std_ulogic_vector(15 downto 0);
    host_data_i       : in std_ulogic_vector(15 downto 0);
    host_sel_i        : in std_ulogic_vector(1 downto 0);
    host_busy_o       : out std_ulogic := '0';
    host_ack_o        : out std_ulogic := '0';
    -- kernel port (read & write 8bit)
    kernel_clk_en_i   : in  std_ulogic;
    kernel_req_i      : in  std_ulogic;
    kernel_wr_i       : in  std_ulogic;
    kernel_addr_i     : in  std_ulogic_vector(17 downto 0);
    kernel_data_i     : in  std_ulogic_vector(7 downto 0);
    kernel_data_o     : out std_ulogic_vector(7 downto 0);
    kernel_busy_o     : out std_ulogic;
    kernel_ack_o      : out std_ulogic;
    -- character ROM and VRAM shares Address & Data-bus
    chr_rom_addr_i    : in  std_ulogic_vector(8 downto 0);
    chr_rom_data_o    : out std_ulogic_vector(7 downto 0);
    chr_rom_ena_i     : in  std_ulogic;
    chr_rom_busy_o    : out std_ulogic;

    -- video port (only read 8bit)
    rd_req_i          : in  std_ulogic;
    rd_addr_i         : in  std_ulogic_vector(17 downto 0);
    rd_data_o         : out std_ulogic_vector(7 downto 0);
    rd_busy_o         : out std_ulogic;
    rd_ack_o          : out std_ulogic;
    -- SRAM / ROM control signals
   
    SRAM_nCS0    : out std_logic;
    SRAM_nCS1   : out std_logic;       
	SRAM_ADDR   : out std_logic_vector(17 downto 0);	
    SRAM_DB     : inout std_logic_vector(15 downto 0);
    SRAM_nWR    : out std_logic;
    SRAM_nOE    : out std_logic;
	SRAM_nBHE	: out std_logic;						
	SRAM_nBLE	: out std_logic;
    -- ext. ROM signals
    rom_ena_o         : out std_ulogic;	
	--------------------------
    -- Monitoring (Debug) signals
    --------------------------
    monitoring_o : out std_ulogic_vector(nr_mon_sigs_c-1 downto 0)
	
  );
end;

architecture rtl of gdp_vram is


  type state_t is(idle_e,kernel_read_e,kernel_write_e,vid_read_e,rom_read_e,host_read_e);
  signal wr_data                            : std_ulogic_vector(15 downto 0);
  signal rd_data, next_rd_data              : std_ulogic_vector(7 downto 0);
  signal set_rd_data                        : std_ulogic;
  signal kernel_data, next_kernel_data      : std_ulogic_vector(7 downto 0);
  signal set_kernel_data                    : std_ulogic;
  signal state,next_state                   : state_t;
  signal ram_address, next_ram_address      : std_ulogic_vector(17 downto 0);
  signal set_ram_address                    : std_ulogic;
  signal kernel_req_pend                    : std_ulogic;
  signal next_kernel_ack                    : std_ulogic;
  signal rd_pend                            : std_ulogic;
  signal next_rd_ack                        : std_ulogic;
  signal ram_wren, next_ram_wren            : std_ulogic;
  signal srom_en, rom_en, next_rom_en       : std_ulogic;
  signal next_rom_ack                       : std_ulogic;
  signal srom_req_pend,rom_req_pend         : std_ulogic;
  signal srom_data,rom_data, next_rom_data  : std_ulogic_vector(7 downto 0);
  signal set_rom_data                       : std_ulogic;
  
  signal sram_data_i_tmp		    : std_ulogic_vector(15 downto 0);
    
  signal ram_cs0, next_ram_cs0          : std_ulogic;
  signal ram_cs1, next_ram_cs1          : std_ulogic;
  signal ram_bhe, next_ram_bhe          : std_ulogic;
  signal ram_ble, next_ram_ble          : std_ulogic;
  signal ram_oe, next_ram_oe          : std_ulogic;
  signal ram_ack					  : std_ulogic;
  
  signal set_host_data  				: std_ulogic;
  signal host_data, next_host_data 		: std_ulogic_vector(15 downto 0);
  signal next_host_ack 					: std_ulogic;
  signal host_req_pend                : std_ulogic;
  
  
begin

  monitoring_o(0)	<= ram_wren;		-- J2
  monitoring_o(1)	<= blank_i;-- J5
  monitoring_o(2)   <= ram_ack; -- J3
  monitoring_o(nr_mon_sigs_c-1 downto 3) <= (others => '0');
	
  
  -- DP-RAM Controller FSM for ext. SRAM/ROM
  -- asynchroner Prozess, definiert die Bedingungen für Übergänge der FSM beim nächsten Clock-Cycle
  process(state, kernel_addr_i,kernel_wr_i,rd_addr_i,
          kernel_req_i,kernel_req_pend,rd_req_i,rd_pend,kernel_clk_en_i,
          sram_data_i_tmp, chr_rom_ena_i, rom_req_pend, blank_i)
          
    procedure do_kernel_acc_p is
    begin
      set_ram_address  <= '1';
      next_ram_address <= "0" & kernel_addr_i(17 downto 1);
      next_ram_wren    <= kernel_wr_i;  
      next_ram_oe      <= not kernel_wr_i;          
      next_ram_ble <=  not kernel_addr_i(0);
      next_ram_bhe <=  kernel_addr_i(0);
      next_ram_cs0 <= '1';	 
      next_ram_cs1 <= '0'; 
      if kernel_wr_i='0' then
        next_state     <= kernel_read_e;
      else
        next_kernel_ack  <= ram_ack;   		        
      end if;
    end procedure;
    
    procedure do_host_acc_p is
    begin
      set_ram_address  <= '1';
      next_ram_address <= host_addr_i(17 downto 0);
      next_ram_wren    <= host_wr_i;
      next_ram_oe      <= not host_wr_i;      
      next_ram_ble <=  not host_sel_i(1);
	  next_ram_bhe <=  not host_sel_i(0);	
      next_ram_cs0 <= not host_addr_i(18);	  
      next_ram_cs1 <= host_addr_i(18);	  
      if host_wr_i='0' then
        next_state     <= host_read_e;
      else
        next_host_ack  <= ram_ack;   		
      end if;
    end procedure;
    
    procedure do_vid_rd_p is
    begin
      set_ram_address  <= '1';
      next_ram_address <= "0" & rd_addr_i(17 downto 1);	      
      next_ram_wren    <= '0';
      next_ram_oe      <= '1'; 
      next_ram_ble <=  not rd_addr_i(0);
      next_ram_bhe <=  rd_addr_i(0);
	  next_ram_cs0 <= '1';	
	  next_ram_cs1 <= '0'; 
      next_state       <= vid_read_e;
    end procedure;
    
    procedure do_rom_acc_p is
    begin
      set_ram_address  <= '1';
      next_ram_address(chr_rom_addr_i'range) <= chr_rom_addr_i;
      next_rom_en      <= '1';
      next_state       <= rom_read_e;
    end procedure;   
    
  begin
    next_state      <= state;
    next_kernel_ack <= '0';
    next_rd_ack     <= '0';
    next_ram_address <= (others => '-');
    set_ram_address  <= '0';
    next_rd_data     <= (others => '-');
    set_rd_data      <= '0';
    next_kernel_data <= (others => '-');
    set_kernel_data  <= '0';
    next_ram_wren    <= '0';
    next_rom_en      <= '0';
    next_rom_data    <= (others => '-');
    set_rom_data     <= '0';
    next_rom_ack     <= '0';
	next_ram_ble	<= '0';
	next_ram_bhe 	<= '0';
	--
	
	next_ram_cs0 <= '0';	
	next_ram_cs1 <= '0';	
	
	set_host_data  <= '0';
	next_host_data <= (others => '-');
	next_host_ack <= '0';


    case state is
      when idle_e =>		
			-- a video read has always a higher priority than a kernel access
			if (rd_req_i or rd_pend)='1' then
			  do_vid_rd_p;
			-- kernel access ...  
			elsif ((kernel_clk_en_i and kernel_req_i) or kernel_req_pend)='1' then
			  do_kernel_acc_p;         
			-- host access ...  
			elsif (host_req_i or host_req_pend)='1' then
			  do_host_acc_p;  
			elsif not INT_CHR_ROM_g and
			 ((kernel_clk_en_i and chr_rom_ena_i) or rom_req_pend)='1' then
			  do_rom_acc_p;			 
			end if;	

      when vid_read_e =>
        next_state   <= idle_e;
        next_rd_ack  <= ram_ack;
        set_rd_data  <= '1';
        if(ram_ble = '1') then
          next_rd_data <= sram_data_i_tmp(7 downto 0);
        else
          next_rd_data <= sram_data_i_tmp(15 downto 8);
        end if;
		
        if ((kernel_clk_en_i and kernel_req_i) or kernel_req_pend)='1' then
          do_kernel_acc_p;
        elsif (host_req_i)='1' then
          do_host_acc_p;   
        end if;

        -- kernel read
      when kernel_read_e =>
        next_state   <= idle_e;
        next_kernel_ack  <= ram_ack;
        set_kernel_data  <= '1';
		if(ram_ble = '1') then
          next_kernel_data <= sram_data_i_tmp(7 downto 0);
        else
          next_kernel_data <= sram_data_i_tmp(15 downto 8);
        end if;
		
        if (rd_req_i or rd_pend)='1' then
          do_vid_rd_p;
        elsif (kernel_clk_en_i and kernel_req_i)='1' then
          do_kernel_acc_p;  
        elsif (host_req_i)='1' then
          do_host_acc_p;                            
        end if;
        
        -- host read
      when host_read_e =>
        next_state   <= idle_e;
        next_host_ack  <= ram_ack;
        set_host_data  <= '1';		
        next_host_data <= sram_data_i_tmp;
        
        if (host_req_i or host_req_pend)='1' then
          do_host_acc_p;
        elsif (rd_req_i or rd_pend)='1' then
          do_vid_rd_p;
        elsif (kernel_clk_en_i and kernel_req_i)='1' then
          do_kernel_acc_p;                           
        end if;
        
       
      when rom_read_e =>
        -- FIXME: configurable waitstates for slower ROMs, (use ack, see FIXME at SRAM_MUX )
        if not INT_CHR_ROM_g then
          next_state    <= idle_e;
          next_rom_ack  <= ram_ack;
          set_rom_data  <= '1';
          next_rom_data <= sram_data_i_tmp(7 downto 0);
          if (rd_req_i or rd_pend)='1' then
            do_vid_rd_p;
          end if;
        else
          next_state <= idle_e;
        end if;
            
      when others =>
        next_state <= idle_e;
    end case;
  end process;

  process(clk_i,reset_n_i)
  begin
    if reset_n_i = ResetActive_c then
      state <= idle_e;
      ram_address <= (others => '0');
	  ram_cs0      <= '0';
	  ram_cs1      <= '0';
      rd_data     <= (others => '0');
      kernel_data <= (others => '0');
      ram_wren    <= '0';
      ram_oe <= '0';
      kernel_req_pend <= '0';
      rd_pend     <= '0';
      kernel_ack_o<= '0';
      rd_ack_o    <= '0';
      if not INT_CHR_ROM_g then
        srom_en      <= '0';
        srom_req_pend<= '0';
        srom_data    <= (others => '0');
      end if;
      
      host_data <= (others => '0');
      host_req_pend <= '0';
      host_ack_o<= '0';
	  
    elsif rising_edge(clk_i) then
      if clk_en_i = '1' then
        state       <= next_state;
		
		-- set address, sel lines
        if set_ram_address  = '1' then
          ram_address <= next_ram_address;
          ram_ble <= next_ram_ble;
          ram_bhe <= next_ram_bhe;
        end if;
		
		-- set cs, we, oe lines

		ram_cs0      <= next_ram_cs0;
		ram_cs1      <= next_ram_cs1;
		ram_wren    <= next_ram_wren; 
		ram_oe      <= next_ram_oe;
		
		-- set video read data
        if set_rd_data = '1' then
          rd_data     <= next_rd_data;
        end if;
        
		-- set kernel read data
        if set_kernel_data = '1' then
          kernel_data <= next_kernel_data;
        end if;       
        
        -- set host read data
        if set_host_data = '1' then
          host_data <= next_host_data;
        end if; 
		
		-- set ack                           		
        kernel_ack_o<= next_kernel_ack;
        rd_ack_o    <= next_rd_ack;  
        host_ack_o	<= next_host_ack;

		-- set write data (kernel)
		if (kernel_clk_en_i and kernel_req_i)='1' then
          kernel_req_pend  <= '1';
		  if(kernel_addr_i(0)='0') then                          
             wr_data(7 downto 0) <= kernel_data_i;
		  else              
             wr_data(15 downto 8) <= kernel_data_i;			  
		  end if;	
        
        -- set write data (host)
		elsif (host_req_i)='1' then
          host_req_pend  <= '1';		                         
          wr_data <= host_data_i;		  
        end if;
		

        if not INT_CHR_ROM_g then
          srom_en    <= next_rom_en;
          if set_rom_data = '1' then
            srom_data  <= next_rom_data;
          end if;
        end if;        	
        
        if next_kernel_ack ='1' then
          kernel_req_pend   <= '0';
        end if;        
        
        if next_host_ack ='1' then
          host_req_pend   <= '0';
        end if; 
        
        if rd_req_i='1' then
          rd_pend   <= '1';
        end if;
        if next_rd_ack ='1' then
          rd_pend   <= '0';
        end if;
        
        if not INT_CHR_ROM_g then
          if (kernel_clk_en_i and chr_rom_ena_i)='1' then
            srom_req_pend <= '1';
          end if;
          if next_rom_ack ='1' then
            srom_req_pend <= '0';
          end if;
        end if;
      end if;	
    end if;
  end process;

  rom_en <= srom_en when not INT_CHR_ROM_g else
            '0';
  rom_req_pend <= srom_req_pend when not INT_CHR_ROM_g else
            '0';
  rom_data <= srom_data when not INT_CHR_ROM_g else
            (others =>'0');

  
  rd_busy_o       <= rd_pend;
  rd_data_o       <= rd_data;
  
  kernel_busy_o   <= kernel_req_pend;
  kernel_data_o   <= kernel_data;
  
  chr_rom_data_o  <= rom_data;
  chr_rom_busy_o  <= rom_req_pend;
  
  host_busy_o   <= host_req_pend;
  host_data_o   <= host_data;
	
    SRAM_nCS0    	<= not ram_cs0;
    SRAM_nCS1    	<= not ram_cs1;
	SRAM_ADDR    	<= std_logic_vector(ram_address);	
    SRAM_nWR    	<= not ram_wren;
    SRAM_nOE    	<= not ram_oe;
	SRAM_nBHE		<= not ram_bhe;						
	SRAM_nBLE		<= not ram_ble;
	
	SRAM_DB  <= std_logic_vector(wr_data) after 1 ns when ((ram_cs0 or ram_cs1) and ram_wren)='1' else  (others => 'Z') after 1 ns;	
    sram_data_i_tmp <= std_ulogic_vector(SRAM_DB);  
    
    ram_ack <= '1';         
  --

  rom_ena_o <= '0';
end architecture rtl;
