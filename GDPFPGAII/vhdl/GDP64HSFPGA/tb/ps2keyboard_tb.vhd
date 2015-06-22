--
-- Copyright 1991-2014 Mentor Graphics Corporation
--
-- All Rights Reserved.
--
-- THIS WORK CONTAINS TRADE SECRET AND PROPRIETARY INFORMATION WHICH IS THE PROPERTY OF 
-- MENTOR GRAPHICS CORPORATION OR ITS LICENSORS AND IS SUBJECT TO LICENSE TERMS.
--   
library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.gdp_global.all;
--use Work.Types.all;

entity test_ps2 is
--    PORT ( count : BUFFER bit_vector(8 downto 1));
end;

architecture only of test_ps2 is

COMPONENT ps2keyboard
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
END COMPONENT ;

SIGNAL clk   : std_ulogic := '0';
signal cpu_clk : std_ulogic := '1';
SIGNAL nreset : std_ulogic := '1';

SIGNAL ps2clk : std_logic := '0';
SIGNAL ps2dat : std_logic := '0';
SIGNAL key_cs : std_ulogic := '0';
SIGNAL dip_cs : std_ulogic := '0';
SIGNAL kopt_cs : std_ulogic := '0';
SIGNAL rd : std_ulogic := '0';
SIGNAL wr : std_ulogic := '0';
SIGNAL data_in : std_ulogic_vector(7 downto 0) := (others => '0');
SIGNAL data_out : std_ulogic_vector(7 downto 0) := (others => '0');
SIGNAL is68000 : std_ulogic := '0';
SIGNAL debug :  std_ulogic_vector(nr_mon_sigs_c-1 downto 0) := (others => '0');


 SIGNAL data_i : std_logic_vector(7 downto 0);
 SIGNAL parity_i : std_logic;
 SIGNAL stop_i : std_logic;

begin

dut : ps2keyboard 
   PORT MAP (
   			reset_n_i    => nreset,
    		clk_i        => clk,
    		--------------------------
    		-- PS/2 Keyboard signals
    		--------------------------
    		-- PS/2 clock line. Bidirectional (resolved!) for Inhibit bus state on
    		-- PS/2 bus. In all other cases an input would be sufficient.
    		Ps2Clk_io    => ps2clk,
    		-- PS/2 data line. Bidirectional for reading and writing data.
    		Ps2Dat_io    => ps2dat,
    		----------------------------------
    		-- Data Bus
    		----------------------------------
    		KeyCS_i   => key_cs,
    		DipCS_i   => dip_cs,
    		KOptCS_i  => kopt_cs,
    		Rd_i      => rd,
    		Wr_i	  => wr,
    		DataIn_i  => data_in,
    		DataOut_o => data_out,
			
			is68000_o => is68000,
    		--------------------------
    		-- Monitoring (Debug) signals
    		--------------------------
    		monitoring_o => debug 
    	);

	clock : PROCESS
	   begin
	   wait for 10 ns; clk  <= not clk;
	end PROCESS clock;
	
	cpu_clock : PROCESS
		begin
		wait for 40 ns; cpu_clk <= not cpu_clk;
	end PROCESS cpu_clock;
	
	stimulus : PROCESS
	   begin
	   wait for 5 ns; nreset  <= '0';
	   wait for 4 ns; nreset  <= '1';
	   wait;
	end PROCESS stimulus;

  --	Ps2Clk <= 'H';
  --	Ps2Dat <= 'H';
 
  	key_cs <= '0';
  	dip_cs <= '0';
  	kopt_cs <= '0';
  	rd <= '0';
  	wr <= '0';
  	data_in <= (others => '0'); 	
  	
  	  -- waveform generation
  	PS2_Keyboard_Device: process
  	 
  	  
  	  procedure sendbit(b : in std_logic) is
  	  begin
  	    --report "sendbit: " & to_string(b) severity note;
  	    Ps2Dat <= b;
  	    Ps2Clk <= '0';
  	    wait for 25 us;
  	    Ps2Clk <= '1';
  	    wait for 25 us;
  	  end sendbit;
  	  procedure sendkbd(data : in std_logic_vector(7 downto 0)) is
  	    variable parity : std_logic :='1';
  	    variable tmp_v : std_logic_vector(7 downto 0);
  	  begin
  	    tmp_v := data;
  	    report "sendkbd: 0x" & to_hex_string(tmp_v) severity note;
  	    --report "sendkbd: 0x" & to_hstring(tmp_v) severity note;
  	    Ps2Dat <= 'H';
  	    Ps2Clk <= 'H';
  	    
  	    wait until  Ps2Clk = 'H' and Ps2Dat = 'H';	-- wait for hosts ready
  	    
  	    sendbit('0');
  	    for i in 0 to 7 loop
  	      sendbit(data(i));
  	      parity := parity xor data(i);
  	    end loop;
  	    sendbit(parity);
  	    sendbit('1');
  	    Ps2Clk <= 'Z';
  	    Ps2Dat <= 'Z';
  	  end sendkbd;
  	  
  	  procedure receivekbd(
  	  	signal data : out std_logic_vector(7 downto 0);
  	  	signal stop : out std_logic;
  	  	signal par  : out std_logic) is
  	  begin
  	    Ps2Dat <= 'H';
  	    report "receivekbd: wait for startbit... " severity note;
  	  	wait until Ps2Clk = 'Z' and Ps2Dat = '0';	-- wait for hosts Start-Bit
  	  	
  	  	
  	  	Ps2Clk <= '1';								-- start transmission
  	  	wait for 25 us;  	  		  	  	
  	  		
  	  	report "receivekbd: read in databits... " severity note;
	  	for i in 0 to 7 loop						-- Data-Bits 0...7
  	  		Ps2Clk <= '0';
  	  		wait for 10 us;
  	  		data(i) <= Ps2Dat;
  	  		wait for 15 us;
  	  		Ps2Clk <= '1';
  	  		wait for 25 us;
  	  	end loop;
  	  	report "receivekbd: data = 0x" & to_hex_string(data) severity note;
  	  	
  	  	
  	  	Ps2Clk <= '0';								-- parity
  	    wait for 25 us;
  	    par <= Ps2Dat;
  	    Ps2Clk <= '1';
  	    wait for 25 us;  	 
  	    report "receivekbd: parity = " & to_string(par) severity note;   
  	    
  	    Ps2Clk <= '0';								-- stop
  	    wait for 25 us;
  	    stop <= Ps2Dat;
  	    wait for 10 ns;
  	    Ps2Clk <= '1';
  	    wait for 10 ns;  	    
  	    Ps2Dat <= '0';								-- kbd ack
  	    wait for 25 us;
  	    Ps2Clk <= '0';
  	    wait for 100 us;
  	    
  	    report "receivekbd: stop = " & to_string(stop) severity note; 
  	    
  	    Ps2Clk <= '1';								-- dummy clock
  	    wait for 300 ns;
  	    Ps2Clk <= '0';
  	    wait for 300 ns;
  	    
  	    Ps2Dat <= 'Z';								-- release lines
  	    Ps2Clk <= 'Z';
  	    
  	  end receivekbd;
  	  
  	begin
	  report "Start Keyboard Initial Process..."	;
	 
  	  wait for 100 ns;
  	  
  	  wait until CPU_Clk'event and CPU_Clk='1';
  	
  	  Ps2Clk <= 'Z';
  	  Ps2Dat <= 'Z';
  	  
  	  wait for 20 ns;
	  sendkbd(X"AA");	-- keyboard has finished power up and sends "PWRON_CODE"
	  					--  .. host can initialize the keyboard now...
	  			
      for i in 1 to 5 loop	  					
       receivekbd(data_i,stop_i,parity_i);		-- receive data from pc	 0xF3...0x00...0xF4
       wait for 200 ns;         				
	   sendkbd(X"FA");   -- send ack		
	   wait for 200 ns;
	  end loop;
	  
	  
	  
	  
	  
	  
	  --------------------------------------
	  -- Send keyboard scan codes ....
	  
  	  sendkbd(X"1c"); -- a  MAKE
  	  wait for 1 ms;  	  
  	  sendkbd(X"F0");		-- BREAK
  	  sendkbd(X"1c");
  	  
  	  sendkbd(X"09"); -- F10(09) F5(03)  MAKE
  	  wait for 1 ms;  	  
  	  sendkbd(X"F0");		-- BREAK
  	  sendkbd(X"09");
  	  
  	  
  	  wait for 500 ns;
  	  assert false report "End of simulation" severity failure;
  	  wait;
  	  
  	  sendkbd(X"12"); -- shift
  	  sendkbd(X"1c"); -- A
  	  wait for 1 ms;
  	  sendkbd(X"F0");
  	  sendkbd(X"1c");
  	  sendkbd(X"F0");
  	  sendkbd(X"12"); -- shift relese
  	  sendkbd(X"06"); -- F1
  	  wait for 1 ms;
  	  sendkbd(X"F0");  
  	  sendkbd(X"06"); -- F1  
  	  sendkbd(X"E0");
  	  sendkbd(X"7d"); -- PGUP
  	  wait for 1 ms;
  	  sendkbd(X"E0");
  	  sendkbd(X"F0");  
  	  sendkbd(X"7D");
  	  wait for 1 ms;
  	
  	  assert false report "End of simulation" severity failure;
  	  wait;
  	end process PS2_Keyboard_Device;
  


end only;

