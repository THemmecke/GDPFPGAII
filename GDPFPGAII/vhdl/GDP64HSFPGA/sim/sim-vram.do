vlib work

# compile project files
vcom -2008   ./test_pkg.vhd \
			 ../rtl/Dffdecl-p.vhd \
		     ../rtl/gdp_global-p.vhd \
		     ../rtl/gdp_vram.vhd
			 
             
# compile testbench                
vcom -2008  ../tb/vram_tb.vhd

# Simulation Modul 'test_vram' laden, Einheiten in ns
vsim -t ns test_vram
onerror {resume}


# alle Signale im Waveform-Viewer laden, Anzeige in HEX
#add wave -group {PS2}  -radix hex /*  



add wave -group {vram}  -radix hex /dut/*


# für 500ns laufen lassen
#run 500

# bis zum Ende laufen	
run -a
