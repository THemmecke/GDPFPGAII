--------------------------------------------------------------------------------
-- Project     : Single Chip NDR Computer
-- Module      : PS/2 Keyboard - Decoder
-- File        : PS2_Decoder.vhd
-- Description : Architecture for attaching a PS/2 Keyboard to AVR Databus.
--------------------------------------------------------------------------------
-- Author       : Andreas Voggeneder
-- Organisation : FH-Hagenberg
-- Department   : Hardware/Software Systems Engineering
-- Language     : VHDL'87
--------------------------------------------------------------------------------
-- Copyright (c) 2003 by Andreas Voggeneder
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.DffGlobal.all;

entity PS2_Decoder is
  port(
    reset_n_i    : in  std_logic;
    clk_i        : in  std_logic;
    --------------------------
    -- PS/2 Keyboard signals
    --------------------------
    enable_i         : in  std_ulogic;
    ScanCode_i       : in  std_ulogic_vector(7 downto 0);
    ScanCode_stb_i   : in  std_ulogic;
    Leds_o           : out std_ulogic_vector(2 downto  0);
    Leds_stb_o       : out std_ulogic;
    --------------------------
    -- Decoded ASCII
    --------------------------
    NkcCode_o        : out std_ulogic_vector(6 downto 0);
    NkcCode_stb_o    : out std_ulogic
  );
end PS2_Decoder;

architecture rtl of PS2_Decoder is
  
  type fetch_state_t is (SCANCODE_e, NKCCode_e, FETCH1_e, FETCH2_e);

  constant EXT_CODE_c            : std_ulogic_vector(7 downto 0) := X"E0";
  constant SETLED_CODE_c         : std_ulogic_vector(7 downto 0) := X"ED";
  constant ECHO_CODE_c           : std_ulogic_vector(7 downto 0) := X"EE";
  constant BREAK_CODE_c          : std_ulogic_vector(7 downto 0) := X"F0";
 
  -- Scancodes for special keys
  
  constant LSHIFT_KEY_c          : std_ulogic_vector(7 downto 0) := X"12";
  constant RSHIFT_KEY_c          : std_ulogic_vector(7 downto 0) := X"59";
  constant SHIFTLOCK_KEY_c       : std_ulogic_vector(7 downto 0) := X"58";
  constant LCTRL_KEY_c           : std_ulogic_vector(7 downto 0) := X"14";
  constant LALT_KEY_c            : std_ulogic_vector(7 downto 0) := X"11";
  constant NUM_KEY_c             : std_ulogic_vector(7 downto 0) := X"77";

 
  constant LSHIFT_STATE_c          : natural := 0;
  constant RSHIFT_STATE_c          : natural := 1;
  constant SHIFTLOCK_STATE_c       : natural := 2;
  constant SHIFT_STATE_MASK_c      : std_ulogic_vector(7 downto 0) := X"07";
  constant LCTRL_STATE_c           : natural := 3;
  constant RCTRL_STATE_c           : natural := 4;
  constant CTRL_STATE_MASK_c       : std_ulogic_vector(7 downto 0) := X"18";
  constant LALT_STATE_c            : natural := 5; -- bit position
  constant RALT_STATE_c            : natural := 6; -- bit position
  constant RALT_STATE_MASK_c	   : std_ulogic_vector(7 downto 0) := X"40";
  constant ALT_STATE_MASK_c        : std_ulogic_vector(7 downto 0) := X"60";
  constant NUMLOCK_STATE_c         : natural := 7;
 
  constant ScanCodesNormal_c     : natural := 0;
  constant ScanCodesShift_c      : natural := 104;
  constant ScanCodesNumeric_c    : natural := ScanCodesShift_c      + 105;
  constant ScanCodesNumericNum_c : natural := ScanCodesNumeric_c    + 24;
  constant SpecialCodes_c        : natural := ScanCodesNumericNum_c + 24;
  constant SpecialCodesCtrl_c    : natural := SpecialCodes_c        + 50;
  --TH
  constant ScanCodesRAlt_c    	 : natural := SpecialCodesCtrl_c    + 44;
  constant ScanCodesLAlt_c    	 : natural := ScanCodesRAlt_c    + 104;
 
--static const PBYTE ScanCodesNormal [] =
--  type ScanCode_ARRAY_t is array(0 to 344) of std_ulogic_vector(7 downto 0);
type ScanCode_ARRAY_t is array(0 to 558) of std_ulogic_vector(7 downto 0);
  constant ScanCode_ARRAY_c : ScanCode_ARRAY_t := (

--  -00-   -01-,  -02-,  -03-,  -04-,  -05-,  -06-,  -07-,  -08-,  -09-,  -0A-,  -0B-,  -0C-,  -0D-,  -0E-,  -0F-
--  -??-   -F9-,  -F7-,  -F5-,  -F3-,  -F1-,  -F2-,  -F12-, -F11-  -F10-  -F8-,  -F6-,  -F4-,  -TAB-  -'^'-,  -0F-      F7: remap 0x83->0x02, F11: remap 0x78->0x08
    X"00", X"92", X"90", X"8E", X"8C", X"8A", X"8B", X"95", X"94", X"93", X"91", X"8F", X"8D", X"09", X"5E", X"00",
--  -10-,  -11-,  -12-,  -13-,  -14-,  -15-,  -16-,  -17-,  -18-,  -19-,  -1A-,  -1B-,  -1C-,  -1D-,  -1E-,  -1F-
--  -10-,  -11-,  -12-,  -13-,  -14-,  -15-,  -'1'-,  -17-,  -18-,  -19-,  -1A-,  -1B-,  -1C-,  -1D-,  -'2'-,  -1F-
    X"00", X"00", X"00", X"00", X"00", X"71", X"31", X"00", X"00", X"00", X"79", X"73", X"61", X"77", X"32", X"00",
--  -20-,  -21-,  -22-,  -23-,  -24-,  -25-,  -26-,  -27-,  -28-,  -29-,  -2A-,  -2B-,  -2C-,  -2D-,  -2E-,  -2F-
--  -20-,  -21-,  -22-,  -23-,  -24-,  -'4'-,  -'3'-,  -27-,  -28-,  -29-,  -2A-,  -2B-,  -2C-,  -2D-,  -'5'-,  -2F-
    X"00", X"63", X"78", X"64", X"65", X"34", X"33", X"00", X"00", X"20", X"76", X"66", X"74", X"72", X"35", X"00",
--  -30-,  -31-,  -32-,  -33-,  -34-,  -35-,  -36-,  -37-,  -38-,  -39-,  -3A-,  -3B-,  -3C-,  -3D-,  -3E-,  -3F-
--  -30-,  -31-,  -32-,  -33-,  -34-,  -35-,  -'6'-,  -37-,  -38-,  -39-,  -3A-,  -3B-,  -3C-,  -3D-,  -3E-,  -3F-
    X"00", X"6E", X"62", X"68", X"67", X"7A", X"36", X"00", X"00", X"00", X"6D", X"6A", X"75", X"37", X"38", X"00",
--  -40-,  -41-,  -42-,  -43-,  -44-,  -45-,  -46-,  -47-,  -48-,  -49-,  -4A-,  -4B-,  -4C-,  -4D-,  -4E-,  -4F-
--  -40-,  -41-,  -42-,  -43-,  -44-,  -45-,  -46-,  -47-,  -48-,  -49-,  -4A-,  -4B-,  -4C-,  -4D-,  -4E-,  -4F-
    X"00", X"2C", X"6B", X"69", X"6F", X"30", X"39", X"00", X"00", X"2E", X"2D", X"6C", X"7C", X"70", X"7E", X"00",
--  -50-,  -51-,  -52-,  -53-,  -54-,  -55-,  -56-,  -57-,  -58-,  -59-,  -5A-,  -5B-,  -5C-,  -5D-,  -5E-,  -5F-
--  -50-,  -51-,  -52-,  -53-,  -54-,  -55-,  -56-,  -57-,  -58-,  -59-,  -5A-,  -5B-,  -5C-,  -5D-,  -5E-,  -5F-
    X"00", X"00", X"7B", X"00", X"7D", X"60", X"00", X"00", X"00", X"00", X"0D", X"2B", X"00", X"23", X"00", X"00",
--  -60-,  -61-,  -62-,  -63-,  -64-,  -65-,  -66-,  -67-,
--  -60-,  -61-,  -62-,  -63-,  -64-,  -65-,  -66-,  -67-,s
    X"00", X"3C", X"00", X"00", X"00", X"00", X"7F", X"00",

--static const PBYTE ScanCodesShift [] =
-- + Offset 0x68 (104)
--  -00-,  -01-,  -02-,  -03-,  -04-,  -05-,  -06-,  -07-,  -08-,  -09-,  -0A-,  -0B-,  -0C-,  -0D-,  -0E-,  -0F-
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"98", X"40", X"00",
--  -10-,  -11-,  -12-,  -13-,  -14-,  -15-,  -16-,  -17-,  -18-,  -19-,  -1A-,  -1B-,  -1C-,  -1D-,  -1E-,  -1F-
    X"00", X"00", X"00", X"00", X"00", X"51", X"21", X"00", X"00", X"00", X"59", X"53", X"41", X"57", X"22", X"00",
--  -20-,  -21-,  -22-,  -23-,  -24-,  -25-,  -26-,  -27-,  -28-,  -29-,  -2A-,  -2B-,  -2C-,  -2D-,  -2E-,  -2F-
    X"00", X"43", X"58", X"44", X"45", X"24", X"23", X"00", X"00", X"20", X"56", X"46", X"54", X"52", X"25", X"00",
--  -30-,  -31-,  -32-,  -33-,  -34-,  -35-,  -36-,  -37-,  -38-,  -39-,  -3A-,  -3B-,  -3C-,  -3D-,  -3E-,  -3F-
    X"00", X"4E", X"42", X"48", X"47", X"5A", X"26", X"00", X"00", X"00", X"4D", X"4A", X"55", X"2F", X"28", X"00",
--  -40-,  -41-,  -42-,  -43-,  -44-,  -45-,  -46-,  -47-,  -48-,  -49-,  -4A-,  -4B-,  -4C-,  -4D-,  -4E-,  -4F-
    X"00", X"3B", X"4B", X"49", X"4F", X"3D", X"29", X"00", X"00", X"3A", X"5F", X"4C", X"5C", X"50", X"3F", X"00",
--  -50-,  -51-,  -52-,  -53-,  -54-,  -55-,  -56-,  -57-,  -58-,  -59-,  -5A-,  -5B-,  -5C-,  -5D-,  -5E-,  -5F-
    X"00", X"00", X"5B", X"00", X"5D", X"60", X"00", X"00", X"00", X"00", X"0D", X"2A", X"00", X"27", X"00", X"00",
--  -60-,  -61-,  -62-,  -63-,  -64-,  -65-,  -66-,  -67-,  -68-,
    X"00", X"3E", X"00", X"00", X"00", X"00", X"7F", X"00", X"00",

--static const PBYTE ScanCodesNumeric [] =
-- + Offset 0xD1 (104+105)
--  -68-,  -69-,  -6A-,  -6B-,  -6C-,  -6D-,  -6E-,  -6F-
    X"00", X"87", X"00", X"13", X"86", X"00", X"00", X"00",
--  -70-,  -71-,  -72-,  -73-,  -74-,  -75-,  -76-,  -77-,  -78-,  -79-,  -7A-,  -7B-,  -7C-,  -7D-,  -7E-,  -7F-
--  -70-,  -71-,  -72-,  -73-,  -74-,  -75-,  -ESC-,  -77-,  -78-,  -79-,  -7A-,  -7B-,  -7C-,  -7D-,  -7E-,  -7F-
    X"88", X"89", X"83", X"00", X"81", X"82", X"1B", X"00", X"00", X"2B", X"85", X"2D",  X"2A", X"84", X"00", X"00",


--static const PBYTE ScanCodesNumericNum [] =
-- + Offset 0xE9 (104+105+24)
-- -68-,  -69-,  -6A-,  -6B-,  -6C-,  -6D-,  -6E-,  -6F-
   X"00", X"31", X"00", X"34", X"37", X"00", X"00", X"00",
--  -70-,  -71-,  -72-,  -73-,  -74-,  -75-,  -76-,  -77-,  -78-,  -79-,  -7A-,  -7B-,  -7C-,  -7D-,  -7E-,  -7F-
   X"30", X"2C", X"32", X"35", X"36", X"38", X"1B", X"00", X"00", X"2B", X"33", X"2D", X"2A", X"39", X"00", X"00",
 
-- static const PBYTE SpecialCodes [] =
-- + Offset 0x101 (104+105+24+24)
  X"13", X"00",      -- -80-    Move cursor left     / Arrow left
  X"04", X"00",      -- -81-    Move cursor right    / Arrow right
  X"05", X"00",      -- -82-    Move cursor up       / Arrow up
  X"18", X"00",      -- -83-    Move cursor down     / Arrow down
  X"12", X"00",      -- -84-    Page up              / Page up
  X"03", X"00",      -- -85-    Page down            / Page down
  X"11", X"53",      -- -86-    Start of line        / Home
  X"11", X"44",      -- -87-    End of Line          / End
  X"16", X"00",      -- -88-    Insert/Override      / Insert
  X"07", X"00",      -- -89-    Delete right char    / Delete

  X"0A", X"00",      -- -8A-    Help                 / F1
  X"0B", X"41",      -- -8B-    Assembler            / F2
  X"0C", X"00",      -- -8C-    Repeat Search        / F3
  X"11", X"46",      -- -8D-    Search               / F4
  X"0B", X"42",      -- -8E-    Block begin          / F5
  X"0B", X"4B",      -- -8F-    Block end            / F6
  X"0B", X"43",      -- -90-    Copy Block           / F7
  X"0B", X"56",      -- -91-    Move Block           / F8
  X"10", X"00",      -- -92-    Characterset         / F9
  X"0B", X"58",      -- -93-    End Editor           / F10
  X"0B", X"78",      -- -94-                         / F11 --TH
  X"0B", X"07",      -- -95-                         / F12 --TH
-- TH
  X"0B", X"15",      -- -96-                         / @
  X"0B", X"4E",      -- -97-                         / \
  X"0B", X"09",	     -- -98-			     / Shift+TAB


-- NKC codes with ctrl key
--static const PBYTE SpecialCodesCtrl [] =
-- + Offset 0x12D (104+105+24+24+46)

  X"01", X"00",      -- -80-    Move word left       / Arrow left
  X"06", X"00",      -- -81-    Move word right      / Arrow right
  X"1A", X"00",      -- -82-    Move line up         / Arrow up
  X"17", X"00",      -- -83-    Move line down       / Arrow down
  X"11", X"45",      -- -84-    Move to top of Page  / Page up
  X"11", X"58",      -- -85-    Move to bottom of page / Page down
  X"11", X"52",      -- -86-    Start of Text        / Home
  X"11", X"43",      -- -87-    End of Text          / End
  X"0E", X"00",      -- -88-    Insert line          / Insert
  X"19", X"00",      -- -89-    Delete line          / Delete

  X"0A", X"00",      -- -8A-    Help                 / F1
  X"0B", X"41",      -- -8B-    Assembler            / F2
  X"0C", X"00",      -- -8C-    Repeat Search        / F3
  X"11", X"41",      -- -8F-    Replace              / F4
  X"0B", X"48",      -- -8E-    Delete blockmarks    / F5
  X"0B", X"59",      -- -8D-    Deleteblock          / F6
  X"11", X"54",      -- -90-    Split line           / F7
  X"11", X"56",      -- -91-    Merge line           / F8
--  X"1B", X"53",      -- -92-    Scrollmode           / F9   1B ist schon durch ESC belegt !! geaendert nach 0B
  X"0B", X"53",      -- -92-    Scrollmode           / F9   1B ist schon durch ESC belegt !! geaendert nach 0B
  X"0B", X"51",      -- -93-    End Editor           / F10
  X"00", X"00",      -- -94-                         / F11
  X"00", X"00",      -- -95-                         / F12
  
--TH- some additional Codes  
-- + Offset 0x159 (104+105+24+24+44+44)

----- ScanCodesRAlt

--  -00-   -01-,  -02-,  -03-,  -04-,  -05-,  -06-,  -07-,  -08-,  -09-,  -0A-,  -0B-,  -0C-,  -0D-,  -0E-,  -0F-
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -10-,  -11-,  -12-,  -13-,  -14-,  -15-,  -16-,  -17-,  -18-,  -19-,  -1A-,  -1B-,  -1C-,  -1D-,  -1E-,  -1F-
--                                       @
    X"00", X"00", X"00", X"00", X"00", X"96", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -20-,  -21-,  -22-,  -23-,  -24-,  -25-,  -26-,  -27-,  -28-,  -29-,  -2A-,  -2B-,  -2C-,  -2D-,  -2E-,  -2F-
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -30-,  -31-,  -32-,  -33-,  -34-,  -35-,  -36-,  -37-,  -38-,  -39-,  -3A-,  -3B-,  -3C-,  -3D-,  -3E-,  -3F-
--                                                                         'µ' 	                '{'   '['      
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"B5", X"00", X"00", X"7B", X"5E", X"00",
--  -40-,  -41-,  -42-,  -43-,  -44-,  -45-,  -46-,  -47-,  -48-,  -49-,  -4A-,  -4B-,  -4C-,  -4D-,  -4E-,  -4F-
--                                      '}'    ']'	                                                    \												                            
    X"00", X"00", X"00", X"00", X"00", X"7D", X"7E", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"97", X"00",
--  -50-,  -51-,  -52-,  -53-,  -54-,  -55-,  -56-,  -57-,  -58-,  -59-,  -5A-,  -5B-,  -5C-,  -5D-,  -5E-,  -5F-
--                                                                                '~'
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"0D", X"7E", X"00", X"00", X"00", X"00",
--  -60-,  -61-,  -62-,  -63-,  -64-,  -65-,  -66-,  -67-,
--         '|'
    X"00", X"7C", X"00", X"00", X"00", X"00", X"00", X"00",
    
----- ScanCodesLAlt
--  -00-   -01-,  -02-,  -03-,  -04-,  -05-,  -06-,  -07-,  -08-,  -09-,  -0A-,  -0B-,  -0C-,  -0D-,  -0E-,  -0F-
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -10-,  -11-,  -12-,  -13-,  -14-,  -15-,  -16-,  -17-,  -18-,  -19-,  -1A-,  -1B-,  -1C-,  -1D-,  -1E-,  -1F-
--                                      
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -20-,  -21-,  -22-,  -23-,  -24-,  -25-,  -26-,  -27-,  -28-,  -29-,  -2A-,  -2B-,  -2C-,  -2D-,  -2E-,  -2F-
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -30-,  -31-,  -32-,  -33-,  -34-,  -35-,  -36-,  -37-,  -38-,  -39-,  -3A-,  -3B-,  -3C-,  -3D-,  -3E-,  -3F-
--                                                                        	                
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -40-,  -41-,  -42-,  -43-,  -44-,  -45-,  -46-,  -47-,  -48-,  -49-,  -4A-,  -4B-,  -4C-,  -4D-,  -4E-,  -4F-
--                                                                                   												                             
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -50-,  -51-,  -52-,  -53-,  -54-,  -55-,  -56-,  -57-,  -58-,  -59-,  -5A-,  -5B-,  -5C-,  -5D-,  -5E-,  -5F-
--                                                                                
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
--  -60-,  -61-,  -62-,  -63-,  -64-,  -65-,  -66-,  -67-,
--     
    X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00"

  );
 
 
 
  signal fetch_state,next_fetch_state : fetch_state_t;
 
  signal next_ExtSeen,ExtSeen             : std_ulogic;
  signal set_ExtSeen                      : std_ulogic;
  signal next_KeyUpSeen,KeyUpSeen         : std_ulogic;
  signal set_KeyUpSeen                    : std_ulogic;
--  signal next_KeyLeds,KeyLeds             : std_ulogic_vector(2 downto 0);
--  signal set_KeyLeds                      : std_ulogic;
  signal next_KeyStates,KeyStates         : std_ulogic_vector(7 downto 0);
  signal set_KeyStates                    : std_ulogic;
  signal setKbdLeds_stb                   : std_ulogic;
  signal next_evaluate,evaluate           : std_ulogic;
  signal next_NKCcode,NKCcode             : std_ulogic_vector(6 downto 0);
  signal set_NKCcode                      : std_ulogic;
  signal next_LookupAddress,LookupAddress : unsigned(8 downto 0);
  signal LookupEn                         : std_ulogic;
  signal LookupData                       : std_ulogic_vector(7 downto 0);
  signal cpu_reset_stb                    : std_ulogic;
--  signal lookup_done              : std_ulogic;
begin

  p_lookup : process(Clk_i)
  begin
    if rising_edge(Clk_i) then
      if LookupEn = '1' then
        LookupData  <= ScanCode_ARRAY_c(to_integer(next_LookupAddress));
      end if;
    end if;
  end process;
  
  

	process (fetch_state,ScanCode_i,ScanCode_stb_i,ExtSeen,KeyUpSeen,KeyStates,evaluate,
	         LookupData,LookupAddress,enable_i)
  	procedure SetKeyState_p(KState : in natural) is
  	begin
  	  next_KeyStates <= KeyStates;
      set_KeyStates  <= '1';
      if KeyUpSeen = '1' then
  	    next_KeyStates(KState) <= '0';
  	    next_KeyUpSeen <= '0';
        set_KeyUpSeen  <= '1';
      else
        next_KeyStates(KState) <= '1';
      end if;
      next_ExtSeen   <= '0';
      set_ExtSeen    <= '1';
	 end SetKeyState_p;
	
	  variable ScanCode_v : std_ulogic_vector(ScanCode_i'range);
	  variable nkcCode_v  : std_ulogic_vector(7 downto 0);
	begin
	  next_fetch_state   <= fetch_state;
    next_ExtSeen       <= '-';
    set_ExtSeen        <= '0';
    next_KeyUpSeen     <= '-';
    set_KeyUpSeen      <= '0';
--    next_KeyLeds       <= (others => '-');
--    set_KeyLeds        <= '0';
    next_KeyStates     <= (others => '-');
    set_KeyStates      <= '0';
    setKbdLeds_stb     <= '0';
    next_NKCcode       <= (others => '-');
    set_NKCcode        <= '0';
    next_LookupAddress <= (others => '-');
    LookupEn           <= '0';
    next_evaluate      <= evaluate; 
    cpu_reset_stb      <= '0';
    
    if enable_i = '1' then
      case fetch_state is
        when NKCCode_e =>
        -- NKC Code read from Lookup-Table
        -- check if subsequent fetches are required
  --    if Lookup_done = '1' then
          nkcCode_v        := LookupData;
          next_fetch_state <= SCANCODE_e;
          if evaluate = '1' then
            next_evaluate <= '0';
            if (unsigned(LookupData) >= X"60" and unsigned(LookupData) <= X"7F" ) then
    --           NKCcode &= 0x1F;                 // Create CTRL-Codes
              nkcCode_v  := "000"&LookupData(4 downto 0);
            elsif (unsigned(LookupData) < X"80" ) then         
              nkcCode_v   := (others => '0');    
              set_NKCcode <= '0';
            end if;
          end if;
          
          if unsigned(nkcCode_v) < X"80"  then
            if unsigned(nkcCode_v) /= 0 then
              next_NKCcode  <= nkcCode_v(next_NKCcode'range);
              set_NKCcode   <= '1';
            end if;     
          else
            if (KeyStates and CTRL_STATE_MASK_c) /= X"00" and
               (KeyStates and ALT_STATE_MASK_c)  /= X"00" and 
              nkcCode_v = X"89" then
              -- Ctrl+Alt+Del detected
              cpu_reset_stb <= '1';
              
            else
    
              nkcCode_v := nkcCode_v(6 downto 0) & "0";
              LookupEn           <= '1';
              next_LookupAddress <= to_unsigned(SpecialCodes_c,9)+ unsigned(nkcCode_v);
              if (KeyStates and CTRL_STATE_MASK_c) /= X"00"  then
                next_LookupAddress <= to_unsigned(SpecialCodesCtrl_c,9)+ unsigned(nkcCode_v);
              end if;
              next_fetch_state <= FETCH1_e;
             end if;
          end if;
          
  
  --    end if;
  
        when FETCH1_e =>
          -- first byte of special codes are fetched
          next_fetch_state <= SCANCODE_e;
          if unsigned(LookupData) /=0 then
            -- write data to fifo
            next_NKCcode       <= LookupData(next_NKCcode'range);
            set_NKCcode        <= '1';
            next_fetch_state   <= FETCH2_e;
            LookupEn           <= '1';
            next_LookupAddress <= LookupAddress + 1;
          end if;
          
        when FETCH2_e =>
        -- second byte of special codes are fetched
          next_fetch_state <= SCANCODE_e;
          if unsigned(LookupData) /=0 then
            -- write data to fifo
            next_NKCcode  <= LookupData(next_NKCcode'range);
            set_NKCcode   <= '1';
          end if;
        
        when SCANCODE_e => 
      		if ScanCode_stb_i = '1' then
            case ScanCode_i is
              when EXT_CODE_c =>
                next_ExtSeen   <= '1';
                set_ExtSeen    <= '1';
              when BREAK_CODE_c =>
                next_KeyUpSeen <= '1';
                set_KeyUpSeen  <= '1';
              when LSHIFT_KEY_c =>
                if ExtSeen = '1' then
                  next_ExtSeen   <= '0';
                  set_ExtSeen    <= '1';
                  next_KeyUpSeen <= '0';
                  set_KeyUpSeen  <= '1';
                else
                  SetKeyState_p(LSHIFT_STATE_c);
                end if;
              when RSHIFT_KEY_c =>
                SetKeyState_p(LSHIFT_STATE_c);
      
              when LCTRL_KEY_c =>
                if ExtSeen = '1' then
                  SetKeyState_p(RCTRL_STATE_c);
                else
                  SetKeyState_p(LCTRL_STATE_c);
                end if;
              
              when LALT_KEY_c =>
                if ExtSeen = '1' then
                  SetKeyState_p(RALT_STATE_c);
                else
                  SetKeyState_p(LALT_STATE_c);
                end if;
              when SHIFTLOCK_KEY_c =>
                if KeyUpSeen = '1' then
                  next_KeyUpSeen <= '0';
                  set_KeyUpSeen  <= '1';
                else
                  next_KeyStates                    <= KeyStates;
                  next_KeyStates(SHIFTLOCK_STATE_c) <= not KeyStates(SHIFTLOCK_STATE_c);
                  set_KeyStates  <= '1';
                  setKbdLeds_stb <= '1';
                end if;
              when NUM_KEY_c =>
                if KeyUpSeen = '1' then
                  next_KeyUpSeen <= '0';
                  set_KeyUpSeen  <= '1';
                else
                  next_KeyStates                    <= KeyStates;
                  next_KeyStates(NUMLOCK_STATE_c)   <= not KeyStates(NUMLOCK_STATE_c);
                  set_KeyStates  <= '1';
                  setKbdLeds_stb <= '1';
                end if;
              when others =>
                ScanCode_v := ScanCode_i;
                case ScanCode_i is
                  when X"83" =>
                    ScanCode_v := X"02";     -- Handle F7 key. Remap scancode
                  when X"78" =>
                    ScanCode_v := X"08";      -- Handle F11 key. Remap scancode
                  when others => null;
                end case;
                
                if KeyUpSeen='1' then
                  next_ExtSeen   <= '0';
                  set_ExtSeen    <= '1';
                  next_KeyUpSeen <= '0';
                  set_KeyUpSeen  <= '1';
                elsif ExtSeen='1' then
                  next_ExtSeen   <= '0';
                  set_ExtSeen    <= '1';
                  -- Handle keys with Extension Code
          		    if ScanCode_v = X"4A" then                 -- '/' key on numeric keypad
          		      nkcCode_v    := X"2F"; 
          		      next_NKCcode <= nkcCode_v(next_NKCcode'range); --'/';
          		      set_NKCcode  <= '1';
          		    elsif ScanCode_v = X"5A" then            -- 'Enter' key on numeric keypad
      --    		       NKCcode = GET_PBYTE(ScanCodesNormal+ScanCode);
                    next_LookupAddress <= to_unsigned(ScanCodesNormal_c,9)+unsigned(ScanCode_v);
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;
                  elsif ( unsigned(ScanCode_v) > X"67" and unsigned(ScanCode_v) < X"80" ) then
      --                 NKCcode = GET_PBYTE(ScanCodesNumeric+ScanCode-0x68);
                    next_LookupAddress <= to_unsigned(ScanCodesNumeric_c,9)+(unsigned(ScanCode_v)-X"68");
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;
                  end if;
                elsif unsigned(ScanCode_v) < X"68" then
                  if (KeyStates and CTRL_STATE_MASK_c)/=X"00" then
      --               NKCcode = GET_PBYTE(ScanCodesNormal+ScanCode);
                     next_LookupAddress <= to_unsigned(ScanCodesNormal_c,9)+unsigned(ScanCode_v);
                     LookupEn           <= '1';
                     next_evaluate      <= '1';
                     next_fetch_state   <= NKCCode_e;
                     
      --               if ( NKCcode >= 0x60 && NKCcode <= 0x7F )
      --                  NKCcode &= 0x1F;                 // Create CTRL-Codes
      --               else if (NKCcode < 0x80 )           
      --                  NKCcode = 0;                     // Everything else but special codes must be zero
                  elsif (KeyStates and SHIFT_STATE_MASK_c)/=X"00" then
      --               NKCcode = GET_PBYTE(ScanCodesShift+ScanCode);
                    next_LookupAddress <= to_unsigned(ScanCodesShift_c,9)+unsigned(ScanCode_v);
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;
--TH-START
       			elsif (KeyStates and RALT_STATE_MASK_c)/=X"00" then      
                    next_LookupAddress <= to_unsigned(ScanCodesRAlt_c,9)+unsigned(ScanCode_v);
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;        
--TH-END           
                  else
      --               NKCcode = GET_PBYTE(ScanCodesNormal+ScanCode);
                    next_LookupAddress <= to_unsigned(ScanCodesNormal_c,9)+unsigned(ScanCode_v);
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;
                  end if;
                elsif unsigned(ScanCode_v) < X"80" then
                  ScanCode_v := std_ulogic_vector(unsigned(ScanCode_v) - X"68");
                  if (KeyStates(NUMLOCK_STATE_c))='1' then
      --               NKCcode = GET_PBYTE(ScanCodesNumericNum+ScanCode);
                    next_LookupAddress <= to_unsigned(ScanCodesNumericNum_c,9)+unsigned(ScanCode_v);
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;
                  else
      --               NKCcode = GET_PBYTE(ScanCodesNumeric+ScanCode);
                    next_LookupAddress <= to_unsigned(ScanCodesNumeric_c,9)+unsigned(ScanCode_v);
                    LookupEn           <= '1';
                    next_evaluate      <= '0';
                    next_fetch_state   <= NKCCode_e;
                  end if;
                end if;
            end case;
          end if;
        when others =>
          next_fetch_state   <= SCANCODE_e;
      end case;
    else
      next_fetch_state   <= SCANCODE_e;
    end if;
	end process;

  seq: process (Clk_i, reset_n_i) is
  begin  -- process
    if reset_n_i = ResetActive_c then
      ExtSeen       <= '0';
      KeyUpSeen     <= '0';
--      KeyLeds       <= (others => '0');
      KeyStates     <= (others => '0');
      NKCcode       <= (others => '0');
      NkcCode_stb_o <= '0';
      evaluate      <= '0';
      fetch_state   <= SCANCODE_e;
      LookupAddress <= (others => '0');
      Leds_stb_o    <= '1';
-- pragma translate_off
      Leds_stb_o    <= '0';
-- pragma translate_on
    elsif rising_edge(Clk_i) then
      fetch_state <= next_fetch_state;
      evaluate    <= next_evaluate; 
--      Lookup_done <= LookupEn;
      if set_ExtSeen = '1' then
        ExtSeen       <= next_ExtSeen;
      end if;
      if set_KeyUpSeen = '1' then
        KeyUpSeen     <= next_KeyUpSeen;
      end if;
--      if set_KeyLeds = '1' then
--        KeyLeds       <= next_KeyLeds;
--      end if;
      Leds_stb_o    <= '0';
      if set_KeyStates = '1' then
        KeyStates     <= next_KeyStates;
        Leds_stb_o    <= '1';
      end if;
      NkcCode_stb_o <= '0';
      if set_NKCcode = '1' then
        NKCcode       <= next_NKCcode;
        NkcCode_stb_o <= '1';
      end if; 
      if LookupEn = '1' then
        LookupAddress <= next_LookupAddress;
      end if;
    end if;
  end process seq;
  
  NkcCode_o     <= NKCcode;
  Leds_o        <= "0" & KeyStates(NUMLOCK_STATE_c) & KeyStates(SHIFTLOCK_STATE_c);
end rtl;