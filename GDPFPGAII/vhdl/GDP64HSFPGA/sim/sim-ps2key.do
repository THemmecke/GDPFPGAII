vlib work

# compile project files
vcom -2008   ./test_pkg.vhd \
			 ../rtl/Dffdecl-p.vhd \
			 ../rtl/InputSync-e.vhd \
			 ../rtl/InputSync-a.vhd \
		     ../rtl/gdp_global-p.vhd \
		     ../rtl/FPGA_specific/ps2_fifo.vhd \
			 ../rtl/PS2_Stuff/ps2_decoder.vhd \
			 ../rtl/PS2_Stuff/ps2_interface.vhd \
			 ../rtl/PS2_Stuff/ps2keyboard.vhd
			 
             
# compile testbench                
vcom -2008  ../tb/ps2keyboard_tb.vhd

# Simulation Modul 'test_ps2' laden, Einheiten in ns
vsim -t ns test_ps2
onerror {resume}


# alle Signale im Waveform-Viewer laden, Anzeige in HEX
#add wave -group {PS2}  -radix hex /*  



add wave -group {PS2}  -radix hex /dut/*
add wave -group {PS2_if}  -radix hex /dut/PS2if/*
add wave -group {PS2_Decoder}  -radix hex /dut/PS2dec/*
add wave -group {FiFo}  -radix hex /dut/PS2FiFo/*

# für 500ns laufen lassen
#run 500

# bis zum Ende laufen	
run -a
