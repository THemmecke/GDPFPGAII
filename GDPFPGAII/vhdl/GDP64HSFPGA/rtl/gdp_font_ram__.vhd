-- generated with romgen by MikeJ
library ieee;
--library unisim;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use unisim.all;

entity gdp_font_ram is
	port (
		Clk			: in  std_ulogic;
		En			: in  std_ulogic;
		Wr			: in  std_ulogic;
		DIn			: in  std_ulogic_vector(7 downto 0);
		Addr	  : in  std_ulogic_vector(9 downto 0);
		Dout		: out std_ulogic_vector(7 downto 0)
	);
end;

architecture RTL of gdp_font_ram is


  type ROM_ARRAY is array(0 to 1023) of std_ulogic_vector(7 downto 0);
  signal IRAM : ROM_ARRAY := (
    -- Standard GDP9366 Character ROM
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"5F", -- 0x0000
    x"00",x"00",x"00",x"03",x"00",x"03",x"00",x"0A", -- 0x0008
    x"1F",x"0A",x"1F",x"0A",x"24",x"2A",x"7F",x"2A", -- 0x0010
    x"12",x"23",x"13",x"08",x"64",x"62",x"36",x"49", -- 0x0018
    x"55",x"22",x"50",x"00",x"00",x"0B",x"07",x"00", -- 0x0020
    x"00",x"1C",x"22",x"41",x"00",x"00",x"41",x"22", -- 0x0028
    x"1C",x"00",x"2A",x"1C",x"7F",x"1C",x"2A",x"08", -- 0x0030
    x"08",x"3E",x"08",x"08",x"00",x"00",x"B0",x"70", -- 0x0038
    x"00",x"08",x"08",x"08",x"08",x"08",x"00",x"60", -- 0x0040
    x"60",x"00",x"00",x"20",x"10",x"08",x"04",x"02", -- 0x0048
    x"3E",x"41",x"41",x"3E",x"00",x"00",x"02",x"7F", -- 0x0050
    x"00",x"00",x"62",x"51",x"49",x"49",x"46",x"41", -- 0x0058
    x"41",x"49",x"4D",x"33",x"0F",x"08",x"08",x"7F", -- 0x0060
    x"08",x"47",x"45",x"45",x"45",x"39",x"3C",x"4A", -- 0x0068
    x"49",x"49",x"30",x"61",x"11",x"09",x"05",x"03", -- 0x0070
    x"36",x"49",x"49",x"49",x"36",x"06",x"49",x"49", -- 0x0078
    x"29",x"1E",x"00",x"36",x"36",x"00",x"00",x"00", -- 0x0080
    x"B6",x"76",x"00",x"00",x"08",x"14",x"22",x"41", -- 0x0088
    x"00",x"14",x"14",x"14",x"14",x"14",x"00",x"41", -- 0x0090
    x"22",x"14",x"08",x"02",x"01",x"51",x"09",x"06", -- 0x0098
    x"3E",x"41",x"5D",x"55",x"5E",x"7E",x"09",x"09", -- 0x00A0
    x"09",x"7E",x"7F",x"49",x"49",x"49",x"36",x"3E", -- 0x00A8
    x"41",x"41",x"41",x"22",x"7F",x"41",x"41",x"41", -- 0x00B0
    x"3E",x"7F",x"49",x"49",x"49",x"41",x"7F",x"09", -- 0x00B8
    x"09",x"09",x"01",x"3E",x"41",x"49",x"49",x"7A", -- 0x00C0
    x"7F",x"08",x"08",x"08",x"7F",x"00",x"41",x"7F", -- 0x00C8
    x"41",x"00",x"20",x"40",x"40",x"40",x"3F",x"7F", -- 0x00D0
    x"08",x"14",x"22",x"41",x"7F",x"40",x"40",x"40", -- 0x00D8
    x"40",x"7F",x"02",x"0C",x"02",x"7F",x"7F",x"02", -- 0x00E0
    x"04",x"08",x"7F",x"3E",x"41",x"41",x"41",x"3E", -- 0x00E8
    x"7F",x"09",x"09",x"09",x"06",x"3E",x"41",x"51", -- 0x00F0
    x"21",x"5E",x"7F",x"09",x"19",x"29",x"46",x"26", -- 0x00F8
    x"49",x"49",x"49",x"32",x"01",x"01",x"7F",x"01", -- 0x0100
    x"01",x"3F",x"40",x"40",x"40",x"3F",x"1F",x"20", -- 0x0108
    x"40",x"20",x"1F",x"7F",x"20",x"18",x"20",x"7F", -- 0x0110
    x"63",x"14",x"08",x"14",x"63",x"07",x"08",x"70", -- 0x0118
    x"08",x"07",x"61",x"51",x"49",x"45",x"43",x"00", -- 0x0120
    x"7F",x"41",x"41",x"00",x"02",x"04",x"08",x"10", -- 0x0128
    x"20",x"00",x"41",x"41",x"7F",x"00",x"04",x"02", -- 0x0130
    x"7F",x"02",x"04",x"08",x"1C",x"2A",x"08",x"08", -- 0x0138
    x"00",x"07",x"0B",x"00",x"00",x"70",x"54",x"54", -- 0x0140
    x"78",x"40",x"40",x"7F",x"44",x"44",x"3C",x"00", -- 0x0148
    x"38",x"44",x"44",x"48",x"38",x"44",x"44",x"7F", -- 0x0150
    x"40",x"00",x"38",x"54",x"54",x"48",x"00",x"08", -- 0x0158
    x"7C",x"0A",x"02",x"00",x"98",x"A4",x"A4",x"7C", -- 0x0160
    x"00",x"7F",x"04",x"04",x"78",x"00",x"00",x"7A", -- 0x0168
    x"00",x"00",x"00",x"40",x"80",x"74",x"00",x"00", -- 0x0170
    x"7E",x"10",x"28",x"44",x"00",x"02",x"7E",x"40", -- 0x0178
    x"00",x"7C",x"04",x"7C",x"04",x"78",x"00",x"7C", -- 0x0180
    x"04",x"04",x"78",x"00",x"38",x"44",x"44",x"38", -- 0x0188
    x"00",x"FC",x"24",x"24",x"18",x"18",x"24",x"24", -- 0x0190
    x"FC",x"80",x"00",x"7C",x"08",x"04",x"04",x"00", -- 0x0198
    x"48",x"54",x"54",x"24",x"00",x"04",x"3E",x"44", -- 0x01A0
    x"20",x"3C",x"40",x"40",x"7C",x"40",x"0C",x"30", -- 0x01A8
    x"40",x"30",x"0C",x"3C",x"40",x"30",x"40",x"3C", -- 0x01B0
    x"44",x"24",x"38",x"48",x"44",x"00",x"1C",x"20", -- 0x01B8
    x"A0",x"FC",x"40",x"64",x"54",x"4C",x"04",x"00", -- 0x01C0
    x"08",x"36",x"41",x"41",x"00",x"00",x"77",x"00", -- 0x01C8
    x"00",x"00",x"41",x"41",x"36",x"08",x"08",x"08", -- 0x01D0
    x"08",x"08",x"38",x"55",x"2A",x"55",x"2A",x"55", -- 0x01D8
    x"FF",x"FF",x"FF",x"FF",x"FF",x"F0",x"F0",x"F0", -- 0x01E0
    x"F0",x"00",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x01E8
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x01F0
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x01F8
    -- Jens modified Character ROM
    x"00",x"00",x"00",x"00",x"00", -- " "
    x"00",x"00",x"5F",x"00",x"00", -- !
    x"00",x"03",x"00",x"03",x"00", -- "
    x"0A",x"1F",x"0A",x"1F",x"0A", -- #
    x"24",x"2A",x"7F",x"2A",x"12", -- $
    x"23",x"13",x"08",x"64",x"62", -- %
    x"36",x"49",x"55",x"22",x"50", -- &
    x"00",x"00",x"0B",x"07",x"00", -- �
    x"00",x"1C",x"22",x"41",x"00", -- (
    x"00",x"41",x"22",x"1C",x"00", -- )
    x"2A",x"1C",x"7F",x"1C",x"2A", -- *
    x"08",x"08",x"3E",x"08",x"08", -- +
    x"00",x"00",x"B0",x"70",x"00", -- ,
    x"08",x"08",x"08",x"08",x"08", -- -
    x"00",x"60",x"60",x"00",x"00", -- .
    x"20",x"10",x"08",x"04",x"02", -- /
    x"3E",x"41",x"41",x"3E",x"00", -- 0
    x"00",x"02",x"7F",x"00",x"00", -- 1
    x"62",x"51",x"49",x"49",x"46", -- 2
    x"41",x"41",x"49",x"4D",x"33", -- 3
    x"0F",x"08",x"08",x"7F",x"08", -- 4
    x"47",x"45",x"45",x"45",x"39", -- 5
    x"3C",x"4A",x"49",x"49",x"30", -- 6
    x"61",x"11",x"09",x"05",x"03", -- 7
    x"36",x"49",x"49",x"49",x"36", -- 8
    x"06",x"49",x"49",x"29",x"1E", -- 9
    x"00",x"36",x"36",x"00",x"00", -- :
    x"00",x"B6",x"76",x"00",x"00", -- ;
    x"08",x"14",x"22",x"41",x"00", -- <
    x"14",x"14",x"14",x"14",x"14", -- =
    x"00",x"41",x"22",x"14",x"08", -- >
    x"02",x"01",x"51",x"09",x"06", -- ?
    x"3E",x"41",x"5D",x"55",x"5E", -- @
    x"7E",x"09",x"09",x"09",x"7E", -- A
    x"7F",x"49",x"49",x"49",x"36", -- B
    x"3E",x"41",x"41",x"41",x"22", -- C
    x"7F",x"41",x"41",x"41",x"3E", -- D
    x"7F",x"49",x"49",x"49",x"41", -- E
    x"7F",x"09",x"09",x"09",x"01", -- F
    x"3E",x"41",x"49",x"49",x"7A", -- G
    x"7F",x"08",x"08",x"08",x"7F", -- H
    x"00",x"41",x"7F",x"41",x"00", -- I
    x"20",x"40",x"40",x"40",x"3F", -- J
    x"7F",x"08",x"14",x"22",x"41", -- K
    x"7F",x"40",x"40",x"40",x"40", -- L
    x"7F",x"02",x"0C",x"02",x"7F", -- M
    x"7F",x"02",x"04",x"08",x"7F", -- N
    x"3E",x"41",x"41",x"41",x"3E", -- O
    x"7F",x"09",x"09",x"09",x"06", -- P
    x"3E",x"41",x"51",x"21",x"5E", -- Q
    x"7F",x"09",x"19",x"29",x"46", -- R
    x"26",x"49",x"49",x"49",x"32", -- S
    x"01",x"01",x"7F",x"01",x"01", -- T
    x"3F",x"40",x"40",x"40",x"3F", -- U
    x"1F",x"20",x"40",x"20",x"1F", -- V
    x"7F",x"20",x"18",x"20",x"7F", -- W
    x"63",x"14",x"08",x"14",x"63", -- X
    x"07",x"08",x"70",x"08",x"07", -- Y
    x"61",x"51",x"49",x"45",x"43", -- Z
    x"7d",x"0a",x"09",x"0a",x"7d", -- �
    x"3d",x"42",x"42",x"42",x"3d", -- �
    x"7d",x"40",x"40",x"40",x"7d", -- �
--    x"00",x"7F",x"41",x"41",x"00", -- [
--    x"02",x"04",x"08",x"10",x"20", -- \
--    x"00",x"41",x"41",x"7F",x"00", -- ]
--    x"04",x"02",x"7F",x"02",x"04", -- Pfeil hoch
    x"04",x"02",x"01",x"02",x"04", -- ^
--    x"08",x"1C",x"2A",x"08",x"08", -- Pfeil links
    x"80",x"80",x"80",x"80",x"80", -- Unterstrich
    x"00",x"07",x"0B",x"00",x"00", -- `
    x"70",x"54",x"54",x"78",x"40", -- a
    x"40",x"7F",x"44",x"44",x"3C", -- b
    x"00",x"38",x"44",x"44",x"48", -- c
    x"38",x"44",x"44",x"7F",x"40", -- d
    x"00",x"38",x"54",x"54",x"48", -- e
    x"00",x"08",x"7C",x"0A",x"02", -- f
    x"00",x"98",x"A4",x"A4",x"7C", -- g
    x"00",x"7F",x"04",x"04",x"78", -- h
    x"00",x"00",x"7A",x"00",x"00", -- i
    x"00",x"40",x"80",x"74",x"00", -- j
    x"00",x"7E",x"10",x"28",x"44", -- k
    x"00",x"02",x"7E",x"40",x"00", -- l
    x"7C",x"04",x"7C",x"04",x"78", -- m
    x"00",x"7C",x"04",x"04",x"78", -- n
    x"00",x"38",x"44",x"44",x"38", -- o
    x"00",x"FC",x"24",x"24",x"18", -- p
    x"18",x"24",x"24",x"FC",x"80", -- q
    x"00",x"7C",x"08",x"04",x"04", -- r
    x"00",x"48",x"54",x"54",x"24", -- s
    x"00",x"04",x"3E",x"44",x"20", -- t
    x"3C",x"40",x"40",x"7C",x"40", -- u
    x"0C",x"30",x"40",x"30",x"0C", -- v
    x"3C",x"40",x"30",x"40",x"3C", -- w
    x"44",x"24",x"38",x"48",x"44", -- x
    x"00",x"1C",x"20",x"A0",x"FC", -- y
    x"40",x"64",x"54",x"4C",x"04", -- z
    x"71",x"54",x"54",x"78",x"41", -- �
    x"00",x"39",x"44",x"44",x"39", -- �
    x"3d",x"40",x"40",x"7d",x"40", -- �
    x"00",x"7f",x"01",x"4d",x"32", -- �
--    x"00",x"08",x"36",x"41",x"41", -- {
--    x"00",x"00",x"77",x"00",x"00", -- |
--    x"00",x"41",x"41",x"36",x"08", -- }
--    x"08",x"08",x"08",x"08",x"38", -- Haken
--    x"04",x"02",x"04",x"08",x"04", -- ~
    x"55",x"2A",x"55",x"2A",x"55", -- Rasterfeld
    x"FF",x"FF",x"FF",x"FF",x"FF", -- 5x5 Block
    x"F0",x"F0",x"F0",x"F0",x"00", -- 4x4 Block
    x"00",x"00",x"00",x"00",x"00", -- 
    x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",
    x"00",x"00",x"00",x"00",x"00",
    x"00",x"00"
    
  );

begin

--  p_rom : process(CLK)
--  begin
--    if rising_edge(CLK) then
--      if clk_en = '1' then
--        D <= ROM(to_integer(unsigned(A)));
--      end if;
--    end if;
--  end process;
  
  process (Clk)
	begin
		if Clk'event and Clk = '1' then
-- pragma translate_off
			if not is_x(Addr) then
-- pragma translate_on
				if En = '1' then
					Dout <= IRAM(to_integer(unsigned(Addr)));
				end if;
-- pragma translate_off
      else
        Dout <= (others =>'-');
			end if;
-- pragma translate_on
			if (En and Wr and Addr(9)) = '1' then
				IRAM(to_integer(unsigned(Addr))) <= DIn;
--				if Int_Addr_r_i = Addr then
--					Dout <= DIn;
--				end if;
			end if;
		end if;
	end process;
  
end RTL;
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  