change working directory to ~\vhdl\GDP64HSFPGA\sim 

do compile-xp_libs.do			 
do compile-gdp.do
do sim.do

run -a

simuliert ca. 15 ms (dauert ca. 10 min). 



modelsim.ini:

; Save the command window contents to this file
TranscriptFile = trnscrpt
default: C:\Programme\altera\14.1


Breakpoint in Source-Code setzen
VSIM> bp lock.vhd 64
oder direkt im SourceWindow 

http://www.ht-lab.com/howto/modelsim/Modelsim_tips.html

set StdArithNoWarnings 1
run 0 ns;
set StdArithNoWarnings 0




Tutorials:
http://www.tkt.cs.tut.fi/tools/public/tutorials/mentor/modelsim/getting_started/gsms.html



--------------------

PS2Keyboard-Only:

do sim-ps2key.do
do sim-vram.do