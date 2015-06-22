-- This file was generated with hex2rom written by Daniel Wallner

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity gdp_font is
	port(
		Clk	     : in std_ulogic;
		clk_en   : in  std_ulogic;
		A	       : in std_ulogic_vector(8 downto 0);
		D	       : out std_ulogic_vector(7 downto 0)
	);
end gdp_font;

architecture rtl of gdp_font is
	signal A_r : std_ulogic_vector(8 downto 0);
begin
	process (Clk)
	begin
		if Clk'event and Clk = '1' then
		  if clk_en = '1' then
			  A_r <= A;
			end if;
		end if;
	end process;
	process (A_r)
	begin
		case to_integer(unsigned(A_r)) is
		when 000000 => D <= "00000000";	-- 0x0000
		when 000001 => D <= "00000000";	-- 0x0001
		when 000002 => D <= "00000000";	-- 0x0002
		when 000003 => D <= "00000000";	-- 0x0003
		when 000004 => D <= "00000000";	-- 0x0004
		when 000005 => D <= "00000000";	-- 0x0005
		when 000006 => D <= "00000000";	-- 0x0006
		when 000007 => D <= "01011111";	-- 0x0007
		when 000008 => D <= "00000000";	-- 0x0008
		when 000009 => D <= "00000000";	-- 0x0009
		when 000010 => D <= "00000000";	-- 0x000A
		when 000011 => D <= "00000011";	-- 0x000B
		when 000012 => D <= "00000000";	-- 0x000C
		when 000013 => D <= "00000011";	-- 0x000D
		when 000014 => D <= "00000000";	-- 0x000E
		when 000015 => D <= "00001010";	-- 0x000F
		when 000016 => D <= "00011111";	-- 0x0010
		when 000017 => D <= "00001010";	-- 0x0011
		when 000018 => D <= "00011111";	-- 0x0012
		when 000019 => D <= "00001010";	-- 0x0013
		when 000020 => D <= "00100100";	-- 0x0014
		when 000021 => D <= "00101010";	-- 0x0015
		when 000022 => D <= "01111111";	-- 0x0016
		when 000023 => D <= "00101010";	-- 0x0017
		when 000024 => D <= "00010010";	-- 0x0018
		when 000025 => D <= "00100011";	-- 0x0019
		when 000026 => D <= "00010011";	-- 0x001A
		when 000027 => D <= "00001000";	-- 0x001B
		when 000028 => D <= "01100100";	-- 0x001C
		when 000029 => D <= "01100010";	-- 0x001D
		when 000030 => D <= "00110110";	-- 0x001E
		when 000031 => D <= "01001001";	-- 0x001F
		when 000032 => D <= "01010101";	-- 0x0020
		when 000033 => D <= "00100010";	-- 0x0021
		when 000034 => D <= "01010000";	-- 0x0022
		when 000035 => D <= "00000000";	-- 0x0023
		when 000036 => D <= "00000000";	-- 0x0024
		when 000037 => D <= "00001011";	-- 0x0025
		when 000038 => D <= "00000111";	-- 0x0026
		when 000039 => D <= "00000000";	-- 0x0027
		when 000040 => D <= "00000000";	-- 0x0028
		when 000041 => D <= "00011100";	-- 0x0029
		when 000042 => D <= "00100010";	-- 0x002A
		when 000043 => D <= "01000001";	-- 0x002B
		when 000044 => D <= "00000000";	-- 0x002C
		when 000045 => D <= "00000000";	-- 0x002D
		when 000046 => D <= "01000001";	-- 0x002E
		when 000047 => D <= "00100010";	-- 0x002F
		when 000048 => D <= "00011100";	-- 0x0030
		when 000049 => D <= "00000000";	-- 0x0031
		when 000050 => D <= "00101010";	-- 0x0032
		when 000051 => D <= "00011100";	-- 0x0033
		when 000052 => D <= "01111111";	-- 0x0034
		when 000053 => D <= "00011100";	-- 0x0035
		when 000054 => D <= "00101010";	-- 0x0036
		when 000055 => D <= "00001000";	-- 0x0037
		when 000056 => D <= "00001000";	-- 0x0038
		when 000057 => D <= "00111110";	-- 0x0039
		when 000058 => D <= "00001000";	-- 0x003A
		when 000059 => D <= "00001000";	-- 0x003B
		when 000060 => D <= "00000000";	-- 0x003C
		when 000061 => D <= "00000000";	-- 0x003D
		when 000062 => D <= "10110000";	-- 0x003E
		when 000063 => D <= "01110000";	-- 0x003F
		when 000064 => D <= "00000000";	-- 0x0040
		when 000065 => D <= "00001000";	-- 0x0041
		when 000066 => D <= "00001000";	-- 0x0042
		when 000067 => D <= "00001000";	-- 0x0043
		when 000068 => D <= "00001000";	-- 0x0044
		when 000069 => D <= "00001000";	-- 0x0045
		when 000070 => D <= "00000000";	-- 0x0046
		when 000071 => D <= "01100000";	-- 0x0047
		when 000072 => D <= "01100000";	-- 0x0048
		when 000073 => D <= "00000000";	-- 0x0049
		when 000074 => D <= "00000000";	-- 0x004A
		when 000075 => D <= "00100000";	-- 0x004B
		when 000076 => D <= "00010000";	-- 0x004C
		when 000077 => D <= "00001000";	-- 0x004D
		when 000078 => D <= "00000100";	-- 0x004E
		when 000079 => D <= "00000010";	-- 0x004F
		when 000080 => D <= "00111110";	-- 0x0050
		when 000081 => D <= "01000001";	-- 0x0051
		when 000082 => D <= "01000001";	-- 0x0052
		when 000083 => D <= "00111110";	-- 0x0053
		when 000084 => D <= "00000000";	-- 0x0054
		when 000085 => D <= "00000000";	-- 0x0055
		when 000086 => D <= "00000010";	-- 0x0056
		when 000087 => D <= "01111111";	-- 0x0057
		when 000088 => D <= "00000000";	-- 0x0058
		when 000089 => D <= "00000000";	-- 0x0059
		when 000090 => D <= "01100010";	-- 0x005A
		when 000091 => D <= "01010001";	-- 0x005B
		when 000092 => D <= "01001001";	-- 0x005C
		when 000093 => D <= "01001001";	-- 0x005D
		when 000094 => D <= "01000110";	-- 0x005E
		when 000095 => D <= "01000001";	-- 0x005F
		when 000096 => D <= "01000001";	-- 0x0060
		when 000097 => D <= "01001001";	-- 0x0061
		when 000098 => D <= "01001101";	-- 0x0062
		when 000099 => D <= "00110011";	-- 0x0063
		when 000100 => D <= "00001111";	-- 0x0064
		when 000101 => D <= "00001000";	-- 0x0065
		when 000102 => D <= "00001000";	-- 0x0066
		when 000103 => D <= "01111111";	-- 0x0067
		when 000104 => D <= "00001000";	-- 0x0068
		when 000105 => D <= "01000111";	-- 0x0069
		when 000106 => D <= "01000101";	-- 0x006A
		when 000107 => D <= "01000101";	-- 0x006B
		when 000108 => D <= "01000101";	-- 0x006C
		when 000109 => D <= "00111001";	-- 0x006D
		when 000110 => D <= "00111100";	-- 0x006E
		when 000111 => D <= "01001010";	-- 0x006F
		when 000112 => D <= "01001001";	-- 0x0070
		when 000113 => D <= "01001001";	-- 0x0071
		when 000114 => D <= "00110000";	-- 0x0072
		when 000115 => D <= "01100001";	-- 0x0073
		when 000116 => D <= "00010001";	-- 0x0074
		when 000117 => D <= "00001001";	-- 0x0075
		when 000118 => D <= "00000101";	-- 0x0076
		when 000119 => D <= "00000011";	-- 0x0077
		when 000120 => D <= "00110110";	-- 0x0078
		when 000121 => D <= "01001001";	-- 0x0079
		when 000122 => D <= "01001001";	-- 0x007A
		when 000123 => D <= "01001001";	-- 0x007B
		when 000124 => D <= "00110110";	-- 0x007C
		when 000125 => D <= "00000110";	-- 0x007D
		when 000126 => D <= "01001001";	-- 0x007E
		when 000127 => D <= "01001001";	-- 0x007F
		when 000128 => D <= "00101001";	-- 0x0080
		when 000129 => D <= "00011110";	-- 0x0081
		when 000130 => D <= "00000000";	-- 0x0082
		when 000131 => D <= "00110110";	-- 0x0083
		when 000132 => D <= "00110110";	-- 0x0084
		when 000133 => D <= "00000000";	-- 0x0085
		when 000134 => D <= "00000000";	-- 0x0086
		when 000135 => D <= "00000000";	-- 0x0087
		when 000136 => D <= "10110110";	-- 0x0088
		when 000137 => D <= "01110110";	-- 0x0089
		when 000138 => D <= "00000000";	-- 0x008A
		when 000139 => D <= "00000000";	-- 0x008B
		when 000140 => D <= "00001000";	-- 0x008C
		when 000141 => D <= "00010100";	-- 0x008D
		when 000142 => D <= "00100010";	-- 0x008E
		when 000143 => D <= "01000001";	-- 0x008F
		when 000144 => D <= "00000000";	-- 0x0090
		when 000145 => D <= "00010100";	-- 0x0091
		when 000146 => D <= "00010100";	-- 0x0092
		when 000147 => D <= "00010100";	-- 0x0093
		when 000148 => D <= "00010100";	-- 0x0094
		when 000149 => D <= "00010100";	-- 0x0095
		when 000150 => D <= "00000000";	-- 0x0096
		when 000151 => D <= "01000001";	-- 0x0097
		when 000152 => D <= "00100010";	-- 0x0098
		when 000153 => D <= "00010100";	-- 0x0099
		when 000154 => D <= "00001000";	-- 0x009A
		when 000155 => D <= "00000010";	-- 0x009B
		when 000156 => D <= "00000001";	-- 0x009C
		when 000157 => D <= "01010001";	-- 0x009D
		when 000158 => D <= "00001001";	-- 0x009E
		when 000159 => D <= "00000110";	-- 0x009F
		when 000160 => D <= "00111110";	-- 0x00A0
		when 000161 => D <= "01000001";	-- 0x00A1
		when 000162 => D <= "01011101";	-- 0x00A2
		when 000163 => D <= "01010101";	-- 0x00A3
		when 000164 => D <= "01011110";	-- 0x00A4
		when 000165 => D <= "01111110";	-- 0x00A5
--    when 000165 => D <= "11111111";	-- 0x00A5
		when 000166 => D <= "00001001";	-- 0x00A6
		when 000167 => D <= "00001001";	-- 0x00A7
		when 000168 => D <= "00001001";	-- 0x00A8
		when 000169 => D <= "01111110";	-- 0x00A9
		when 000170 => D <= "01111111";	-- 0x00AA
		when 000171 => D <= "01001001";	-- 0x00AB
		when 000172 => D <= "01001001";	-- 0x00AC
		when 000173 => D <= "01001001";	-- 0x00AD
		when 000174 => D <= "00110110";	-- 0x00AE
		when 000175 => D <= "00111110";	-- 0x00AF
		when 000176 => D <= "01000001";	-- 0x00B0
		when 000177 => D <= "01000001";	-- 0x00B1
		when 000178 => D <= "01000001";	-- 0x00B2
		when 000179 => D <= "00100010";	-- 0x00B3
		when 000180 => D <= "01111111";	-- 0x00B4
		when 000181 => D <= "01000001";	-- 0x00B5
		when 000182 => D <= "01000001";	-- 0x00B6
		when 000183 => D <= "01000001";	-- 0x00B7
		when 000184 => D <= "00111110";	-- 0x00B8
		when 000185 => D <= "01111111";	-- 0x00B9
		when 000186 => D <= "01001001";	-- 0x00BA
		when 000187 => D <= "01001001";	-- 0x00BB
		when 000188 => D <= "01001001";	-- 0x00BC
		when 000189 => D <= "01000001";	-- 0x00BD
		when 000190 => D <= "01111111";	-- 0x00BE
		when 000191 => D <= "00001001";	-- 0x00BF
		when 000192 => D <= "00001001";	-- 0x00C0
		when 000193 => D <= "00001001";	-- 0x00C1
		when 000194 => D <= "00000001";	-- 0x00C2
		when 000195 => D <= "00111110";	-- 0x00C3
		when 000196 => D <= "01000001";	-- 0x00C4
		when 000197 => D <= "01001001";	-- 0x00C5
		when 000198 => D <= "01001001";	-- 0x00C6
		when 000199 => D <= "01111010";	-- 0x00C7
		when 000200 => D <= "01111111";	-- 0x00C8
		when 000201 => D <= "00001000";	-- 0x00C9
		when 000202 => D <= "00001000";	-- 0x00CA
		when 000203 => D <= "00001000";	-- 0x00CB
		when 000204 => D <= "01111111";	-- 0x00CC
		when 000205 => D <= "00000000";	-- 0x00CD
		when 000206 => D <= "01000001";	-- 0x00CE
		when 000207 => D <= "01111111";	-- 0x00CF
		when 000208 => D <= "01000001";	-- 0x00D0
		when 000209 => D <= "00000000";	-- 0x00D1
		when 000210 => D <= "00100000";	-- 0x00D2
		when 000211 => D <= "01000000";	-- 0x00D3
		when 000212 => D <= "01000000";	-- 0x00D4
		when 000213 => D <= "01000000";	-- 0x00D5
		when 000214 => D <= "00111111";	-- 0x00D6
		when 000215 => D <= "01111111";	-- 0x00D7
		when 000216 => D <= "00001000";	-- 0x00D8
		when 000217 => D <= "00010100";	-- 0x00D9
		when 000218 => D <= "00100010";	-- 0x00DA
		when 000219 => D <= "01000001";	-- 0x00DB
		when 000220 => D <= "01111111";	-- 0x00DC
		when 000221 => D <= "01000000";	-- 0x00DD
		when 000222 => D <= "01000000";	-- 0x00DE
		when 000223 => D <= "01000000";	-- 0x00DF
		when 000224 => D <= "01000000";	-- 0x00E0
		when 000225 => D <= "01111111";	-- 0x00E1
		when 000226 => D <= "00000010";	-- 0x00E2
		when 000227 => D <= "00001100";	-- 0x00E3
		when 000228 => D <= "00000010";	-- 0x00E4
		when 000229 => D <= "01111111";	-- 0x00E5
		when 000230 => D <= "01111111";	-- 0x00E6
		when 000231 => D <= "00000010";	-- 0x00E7
		when 000232 => D <= "00000100";	-- 0x00E8
		when 000233 => D <= "00001000";	-- 0x00E9
		when 000234 => D <= "01111111";	-- 0x00EA
		when 000235 => D <= "00111110";	-- 0x00EB
		when 000236 => D <= "01000001";	-- 0x00EC
		when 000237 => D <= "01000001";	-- 0x00ED
		when 000238 => D <= "01000001";	-- 0x00EE
		when 000239 => D <= "00111110";	-- 0x00EF
		when 000240 => D <= "01111111";	-- 0x00F0
		when 000241 => D <= "00001001";	-- 0x00F1
		when 000242 => D <= "00001001";	-- 0x00F2
		when 000243 => D <= "00001001";	-- 0x00F3
		when 000244 => D <= "00000110";	-- 0x00F4
		when 000245 => D <= "00111110";	-- 0x00F5
		when 000246 => D <= "01000001";	-- 0x00F6
		when 000247 => D <= "01010001";	-- 0x00F7
		when 000248 => D <= "00100001";	-- 0x00F8
		when 000249 => D <= "01011110";	-- 0x00F9
		when 000250 => D <= "01111111";	-- 0x00FA
		when 000251 => D <= "00001001";	-- 0x00FB
		when 000252 => D <= "00011001";	-- 0x00FC
		when 000253 => D <= "00101001";	-- 0x00FD
		when 000254 => D <= "01000110";	-- 0x00FE
		when 000255 => D <= "00100110";	-- 0x00FF
		when 000256 => D <= "01001001";	-- 0x0100
		when 000257 => D <= "01001001";	-- 0x0101
		when 000258 => D <= "01001001";	-- 0x0102
		when 000259 => D <= "00110010";	-- 0x0103
		when 000260 => D <= "00000001";	-- 0x0104
		when 000261 => D <= "00000001";	-- 0x0105
		when 000262 => D <= "01111111";	-- 0x0106
		when 000263 => D <= "00000001";	-- 0x0107
		when 000264 => D <= "00000001";	-- 0x0108
		when 000265 => D <= "00111111";	-- 0x0109
		when 000266 => D <= "01000000";	-- 0x010A
		when 000267 => D <= "01000000";	-- 0x010B
		when 000268 => D <= "01000000";	-- 0x010C
		when 000269 => D <= "00111111";	-- 0x010D
		when 000270 => D <= "00011111";	-- 0x010E
		when 000271 => D <= "00100000";	-- 0x010F
		when 000272 => D <= "01000000";	-- 0x0110
		when 000273 => D <= "00100000";	-- 0x0111
		when 000274 => D <= "00011111";	-- 0x0112
		when 000275 => D <= "01111111";	-- 0x0113
		when 000276 => D <= "00100000";	-- 0x0114
		when 000277 => D <= "00011000";	-- 0x0115
		when 000278 => D <= "00100000";	-- 0x0116
		when 000279 => D <= "01111111";	-- 0x0117
		when 000280 => D <= "01100011";	-- 0x0118
		when 000281 => D <= "00010100";	-- 0x0119
		when 000282 => D <= "00001000";	-- 0x011A
		when 000283 => D <= "00010100";	-- 0x011B
		when 000284 => D <= "01100011";	-- 0x011C
		when 000285 => D <= "00000111";	-- 0x011D
		when 000286 => D <= "00001000";	-- 0x011E
		when 000287 => D <= "01110000";	-- 0x011F
		when 000288 => D <= "00001000";	-- 0x0120
		when 000289 => D <= "00000111";	-- 0x0121
		when 000290 => D <= "01100001";	-- 0x0122
		when 000291 => D <= "01010001";	-- 0x0123
		when 000292 => D <= "01001001";	-- 0x0124
		when 000293 => D <= "01000101";	-- 0x0125
		when 000294 => D <= "01000011";	-- 0x0126
		when 000295 => D <= "00000000";	-- 0x0127
		when 000296 => D <= "01111111";	-- 0x0128
		when 000297 => D <= "01000001";	-- 0x0129
		when 000298 => D <= "01000001";	-- 0x012A
		when 000299 => D <= "00000000";	-- 0x012B
		when 000300 => D <= "00000010";	-- 0x012C
		when 000301 => D <= "00000100";	-- 0x012D
		when 000302 => D <= "00001000";	-- 0x012E
		when 000303 => D <= "00010000";	-- 0x012F
		when 000304 => D <= "00100000";	-- 0x0130
		when 000305 => D <= "00000000";	-- 0x0131
		when 000306 => D <= "01000001";	-- 0x0132
		when 000307 => D <= "01000001";	-- 0x0133
		when 000308 => D <= "01111111";	-- 0x0134
		when 000309 => D <= "00000000";	-- 0x0135
		when 000310 => D <= "00000100";	-- 0x0136
		when 000311 => D <= "00000010";	-- 0x0137
		when 000312 => D <= "01111111";	-- 0x0138
		when 000313 => D <= "00000010";	-- 0x0139
		when 000314 => D <= "00000100";	-- 0x013A
		when 000315 => D <= "00001000";	-- 0x013B
		when 000316 => D <= "00011100";	-- 0x013C
		when 000317 => D <= "00101010";	-- 0x013D
		when 000318 => D <= "00001000";	-- 0x013E
		when 000319 => D <= "00001000";	-- 0x013F
		when 000320 => D <= "00000000";	-- 0x0140
		when 000321 => D <= "00000111";	-- 0x0141
		when 000322 => D <= "00001011";	-- 0x0142
		when 000323 => D <= "00000000";	-- 0x0143
		when 000324 => D <= "00000000";	-- 0x0144
		when 000325 => D <= "01110000";	-- 0x0145
		when 000326 => D <= "01010100";	-- 0x0146
		when 000327 => D <= "01010100";	-- 0x0147
		when 000328 => D <= "01111000";	-- 0x0148
		when 000329 => D <= "01000000";	-- 0x0149
		when 000330 => D <= "01000000";	-- 0x014A
		when 000331 => D <= "01111111";	-- 0x014B
		when 000332 => D <= "01000100";	-- 0x014C
		when 000333 => D <= "01000100";	-- 0x014D
		when 000334 => D <= "00111100";	-- 0x014E
		when 000335 => D <= "00000000";	-- 0x014F
		when 000336 => D <= "00111000";	-- 0x0150
		when 000337 => D <= "01000100";	-- 0x0151
		when 000338 => D <= "01000100";	-- 0x0152
		when 000339 => D <= "01001000";	-- 0x0153
		when 000340 => D <= "00111000";	-- 0x0154
		when 000341 => D <= "01000100";	-- 0x0155
		when 000342 => D <= "01000100";	-- 0x0156
		when 000343 => D <= "01111111";	-- 0x0157
		when 000344 => D <= "01000000";	-- 0x0158
		when 000345 => D <= "00000000";	-- 0x0159
		when 000346 => D <= "00111000";	-- 0x015A
		when 000347 => D <= "01010100";	-- 0x015B
		when 000348 => D <= "01010100";	-- 0x015C
		when 000349 => D <= "01001000";	-- 0x015D
		when 000350 => D <= "00000000";	-- 0x015E
		when 000351 => D <= "00001000";	-- 0x015F
		when 000352 => D <= "01111100";	-- 0x0160
		when 000353 => D <= "00001010";	-- 0x0161
		when 000354 => D <= "00000010";	-- 0x0162
		when 000355 => D <= "00000000";	-- 0x0163
		when 000356 => D <= "10011000";	-- 0x0164
		when 000357 => D <= "10100100";	-- 0x0165
		when 000358 => D <= "10100100";	-- 0x0166
		when 000359 => D <= "01111100";	-- 0x0167
		when 000360 => D <= "00000000";	-- 0x0168
		when 000361 => D <= "01111111";	-- 0x0169
		when 000362 => D <= "00000100";	-- 0x016A
		when 000363 => D <= "00000100";	-- 0x016B
		when 000364 => D <= "01111000";	-- 0x016C
		when 000365 => D <= "00000000";	-- 0x016D
		when 000366 => D <= "00000000";	-- 0x016E
		when 000367 => D <= "01111010";	-- 0x016F
		when 000368 => D <= "00000000";	-- 0x0170
		when 000369 => D <= "00000000";	-- 0x0171
		when 000370 => D <= "00000000";	-- 0x0172
		when 000371 => D <= "01000000";	-- 0x0173
		when 000372 => D <= "10000000";	-- 0x0174
		when 000373 => D <= "01110100";	-- 0x0175
		when 000374 => D <= "00000000";	-- 0x0176
		when 000375 => D <= "00000000";	-- 0x0177
		when 000376 => D <= "01111110";	-- 0x0178
		when 000377 => D <= "00010000";	-- 0x0179
		when 000378 => D <= "00101000";	-- 0x017A
		when 000379 => D <= "01000100";	-- 0x017B
		when 000380 => D <= "00000000";	-- 0x017C
		when 000381 => D <= "00000010";	-- 0x017D
		when 000382 => D <= "01111110";	-- 0x017E
		when 000383 => D <= "01000000";	-- 0x017F
		when 000384 => D <= "00000000";	-- 0x0180
		when 000385 => D <= "01111100";	-- 0x0181
		when 000386 => D <= "00000100";	-- 0x0182
		when 000387 => D <= "01111100";	-- 0x0183
		when 000388 => D <= "00000100";	-- 0x0184
		when 000389 => D <= "01111000";	-- 0x0185
		when 000390 => D <= "00000000";	-- 0x0186
		when 000391 => D <= "01111100";	-- 0x0187
		when 000392 => D <= "00000100";	-- 0x0188
		when 000393 => D <= "00000100";	-- 0x0189
		when 000394 => D <= "01111000";	-- 0x018A
		when 000395 => D <= "00000000";	-- 0x018B
		when 000396 => D <= "00111000";	-- 0x018C
		when 000397 => D <= "01000100";	-- 0x018D
		when 000398 => D <= "01000100";	-- 0x018E
		when 000399 => D <= "00111000";	-- 0x018F
		when 000400 => D <= "00000000";	-- 0x0190
		when 000401 => D <= "11111100";	-- 0x0191
		when 000402 => D <= "00100100";	-- 0x0192
		when 000403 => D <= "00100100";	-- 0x0193
		when 000404 => D <= "00011000";	-- 0x0194
		when 000405 => D <= "00011000";	-- 0x0195
		when 000406 => D <= "00100100";	-- 0x0196
		when 000407 => D <= "00100100";	-- 0x0197
		when 000408 => D <= "11111100";	-- 0x0198
		when 000409 => D <= "10000000";	-- 0x0199
		when 000410 => D <= "00000000";	-- 0x019A
		when 000411 => D <= "01111100";	-- 0x019B
		when 000412 => D <= "00001000";	-- 0x019C
		when 000413 => D <= "00000100";	-- 0x019D
		when 000414 => D <= "00000100";	-- 0x019E
		when 000415 => D <= "00000000";	-- 0x019F
		when 000416 => D <= "01001000";	-- 0x01A0
		when 000417 => D <= "01010100";	-- 0x01A1
		when 000418 => D <= "01010100";	-- 0x01A2
		when 000419 => D <= "00100100";	-- 0x01A3
		when 000420 => D <= "00000000";	-- 0x01A4
		when 000421 => D <= "00000100";	-- 0x01A5
		when 000422 => D <= "00111110";	-- 0x01A6
		when 000423 => D <= "01000100";	-- 0x01A7
		when 000424 => D <= "00100000";	-- 0x01A8
		when 000425 => D <= "00111100";	-- 0x01A9
		when 000426 => D <= "01000000";	-- 0x01AA
		when 000427 => D <= "01000000";	-- 0x01AB
		when 000428 => D <= "01111100";	-- 0x01AC
		when 000429 => D <= "01000000";	-- 0x01AD
		when 000430 => D <= "00001100";	-- 0x01AE
		when 000431 => D <= "00110000";	-- 0x01AF
		when 000432 => D <= "01000000";	-- 0x01B0
		when 000433 => D <= "00110000";	-- 0x01B1
		when 000434 => D <= "00001100";	-- 0x01B2
		when 000435 => D <= "00111100";	-- 0x01B3
		when 000436 => D <= "01000000";	-- 0x01B4
		when 000437 => D <= "00110000";	-- 0x01B5
		when 000438 => D <= "01000000";	-- 0x01B6
		when 000439 => D <= "00111100";	-- 0x01B7
		when 000440 => D <= "01000100";	-- 0x01B8
		when 000441 => D <= "00100100";	-- 0x01B9
		when 000442 => D <= "00111000";	-- 0x01BA
		when 000443 => D <= "01001000";	-- 0x01BB
		when 000444 => D <= "01000100";	-- 0x01BC
		when 000445 => D <= "00000000";	-- 0x01BD
		when 000446 => D <= "00011100";	-- 0x01BE
		when 000447 => D <= "00100000";	-- 0x01BF
		when 000448 => D <= "10100000";	-- 0x01C0
		when 000449 => D <= "11111100";	-- 0x01C1
		when 000450 => D <= "01000000";	-- 0x01C2
		when 000451 => D <= "01100100";	-- 0x01C3
		when 000452 => D <= "01010100";	-- 0x01C4
		when 000453 => D <= "01001100";	-- 0x01C5
		when 000454 => D <= "00000100";	-- 0x01C6
		when 000455 => D <= "00000000";	-- 0x01C7
		when 000456 => D <= "00001000";	-- 0x01C8
		when 000457 => D <= "00110110";	-- 0x01C9
		when 000458 => D <= "01000001";	-- 0x01CA
		when 000459 => D <= "01000001";	-- 0x01CB
		when 000460 => D <= "00000000";	-- 0x01CC
		when 000461 => D <= "00000000";	-- 0x01CD
		when 000462 => D <= "01110111";	-- 0x01CE
		when 000463 => D <= "00000000";	-- 0x01CF
		when 000464 => D <= "00000000";	-- 0x01D0
		
		when 000465 => D <= "00000000";	-- 0x01D1
		when 000466 => D <= "01000001";	-- 0x01D2
		when 000467 => D <= "01000001";	-- 0x01D3
		when 000468 => D <= "00110110";	-- 0x01D4
		when 000469 => D <= "00001000";	-- 0x01D5
		
		when 000470 => D <= "00001000";	-- 0x01D6
		when 000471 => D <= "00001000";	-- 0x01D7
		when 000472 => D <= "00001000";	-- 0x01D8
		when 000473 => D <= "00001000";	-- 0x01D9
		when 000474 => D <= "00111000";	-- 0x01DA
		
		when 000475 => D <= "01010101";	-- 0x01DB
		when 000476 => D <= "00101010";	-- 0x01DC
		when 000477 => D <= "01010101";	-- 0x01DD
		when 000478 => D <= "00101010";	-- 0x01DE
		when 000479 => D <= "01010101";	-- 0x01DF
		-- 5x8 Block
		when 000480 => D <= "11111111";	-- 0x01E0
		when 000481 => D <= "11111111";	-- 0x01E1
		when 000482 => D <= "11111111";	-- 0x01E2
		when 000483 => D <= "11111111";	-- 0x01E3
		when 000484 => D <= "11111111";	-- 0x01E4
		-- 4x4 Block
		when 000485 => D <= "11110000";	-- 0x01E5
		when 000486 => D <= "11110000";	-- 0x01E6
		when 000487 => D <= "11110000";	-- 0x01E7
		when 000488 => D <= "11110000";	-- 0x01E8
		when 000489 => D <= "00000000";	-- 0x01E9
		when others => D <= "--------";
		end case;
	end process;
end;
