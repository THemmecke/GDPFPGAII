-- VHDL module instantiation generated by SCUBA ispLever_v61_SP1_Build (17)
-- Module  Version: 4.1
-- Sat Jun 16 11:26:56 2007

-- parameterized module component declaration
component ps2_fifo
    port (Data: in  std_logic_vector(6 downto 0); Clock: in  std_logic; 
        WrEn: in  std_logic; RdEn: in  std_logic; Reset: in  std_logic; 
        Q: out  std_logic_vector(6 downto 0); Empty: out  std_logic; 
        Full: out  std_logic);
end component;

-- parameterized module component instance
__ : ps2_fifo
    port map (Data(6 downto 0)=>__, Clock=>__, WrEn=>__, RdEn=>__, Reset=>__, 
        Q(6 downto 0)=>__, Empty=>__, Full=>__);