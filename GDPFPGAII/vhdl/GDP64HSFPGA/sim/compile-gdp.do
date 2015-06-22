vlib work
vcom -93  ../tb/gdp_bitmap-p.vhd
#vcom -93 -check_synthesis ../rtl/Dffdecl-p.vhd
vcom -2008                   ../rtl/Dffdecl-p.vhd \
                           ../rtl/InputSync-e.vhd \
                           ../rtl/InputSync-a.vhd \
                           ../rtl/gdp_global-p.vhd \
                           ../rtl/gdp_bi.vhd  \
                           ../rtl/gdp_decoder.vhd \
                           ../rtl/gdp_bresenham.vhd \
                           ../rtl/gdp_font.vhd \
                           ../rtl/gdp_font_ram.vhd \
                           ../rtl/gdp_character.vhd \
                           ../rtl/gdp_vram.vhd \
                           ../rtl/gdp_kernel.vhd \
                           ../rtl/gdp_clut.vhd \
                           ../rtl/gdp_video.vhd \
                           ../rtl/gdp_top.vhd \
                           ../rtl/FPGA_specific/ps2_fifo.vhd \
                           ../rtl/PS2_Stuff/PS2_Interface.vhd \
                           ../rtl/PS2_Stuff/PS2_Decoder.vhd \
                           ../rtl/PS2_Stuff/PS2Keyboard.vhd \
                           ../rtl/PS2_Stuff/PS2Mouse.vhd \
                           ../rtl/Ser/Ser1.vhd \
                           ../rtl/SPI/SPI_Interface.vhd \
                           ../rtl/VDIP/SPI_Vdip.vhd \
                           ../rtl/Timer/Timer.vhd \
                           ../rtl/sound/wf2149ip_pkg.vhd \
                           ../rtl/sound/wf2149ip_wave.vhd \
                           ../rtl/sound/dac.vhd \
                           ../rtl/sound/wf2149ip_top_soc.vhd \
                           ../tb/UART_pkg.vhd \
                           ../tb/TB_Receiver.vhd \
                           ../tb/TB_Sender.vhd \
                           ../tb/sram/package_timing.vhd \
                           ../tb/sram/package_utility.vhd \
                           ../tb/sram/async_512kx16.vhd \
                           ../../gdp_fpgaii-p.vhd \
                           ../../gdp_intercon.vhd \
                           ../../gide.vhd \
                           ../../nkc16_wb_wrapper.vhd \
                           ../../sram.vhd \
                           ../../gdp_fpgaii_top.vhd \
                           ../rtl/gdp_lattice_top.vhd

#vcom -93  ../tb/sram.vhd
vcom -2008  ../tb/gdp_lattice_tb.vhd


vsim -t ps gdp_lattice_tb
onerror {resume}
#add wave  -divider Testbench
add wave -group {TB}  -radix hex /*  
add wave -group {SRAM}  -radix hex /VSRAM0/*
#add wave  -divider Top
add wave -group {Top}  -radix hex /dut/*

add wave -group {NKCIF}  -radix hex /dut/NKCIF/*
add wave -group {Intercon}  -radix hex /dut/INTERCON/*
add wave -group {SRAM1}  -radix hex /dut/SRAM1/*
add wave -group {GDPHS}  -radix hex /dut/GDPHS/*

#add wave  -divider Businterface
add wave -group {Businterface}  -radix hex /dut/GDPHS/bi_inst/*
add wave  -divider Keyboard
add wave -group {PS2}  -radix hex /dut/GDPHS/impl_key2/kbd/*
#add wave  -divider PS2_if
add wave -group {PS2_if}  -radix hex /dut/GDPHS/impl_key2/kbd/PS2if/*
#add wave  -divider PS2_Decoder
add wave -group {PS2_Decoder}  -radix hex /dut/GDPHS/impl_key2/kbd/PS2dec/*
add wave  -divider Mouse
add wave -group {Mouse}  -radix hex /dut/GDPHS/impl_mouse/mouse/*
add wave -group {Mouse}  -radix hex /dut/GDPHS/impl_mouse/mouse/nkc_mouse/*
#add wave  -divider Mouse_PS2_if
add wave -group {Mouse_PS2_IF}  -radix hex /dut/GDPHS/impl_mouse/mouse/PS2if/*
add wave  -divider
#add wave  -divider SPI
add wave -group {SPI}  -radix hex /dut/GDPHS/impl_spi/spi/*
#add wave  -divider VDIP
#add wave -group {VDIP}  -radix hex /dut/GDPHS/impl_vdip/vdip/*
#add wave  -divider Ser1
add wave -group {Ser1}  -radix hex /dut/GDPHS/impl_ser1/ser/*
#add wave  -divider TB-Receiver
add wave -group {TB_Receiver}  -radix hex /rx/*
add wave  -divider GDP
add wave -group {GDPHS_TOP}  -radix hex /dut/GDPHS/GDP/*
#add wave  -divider Video
add wave -group {Video}  -radix hex /dut/GDPHS/gdp/video/*
#add wave  -divider CLUT
add wave -group {CLUT}  -radix hex /dut/GDPHS/gdp/video/use_clut/clut_inst/*
#add wave  -divider Decoder
add wave -group {Decoder}  -radix hex /dut/GDPHS/gdp/kernel/dec/*
#add wave  -divider Kernel
add wave -group {Kernel}  -radix hex /dut/GDPHS/gdp/kernel/*
#add wave  -divider Bresenham
add wave -group {Bresenham}  -radix hex /dut/GDPHS/gdp/kernel/bres/*
#add wave  -divider Character
add wave -group {Char}  -radix hex /dut/GDPHS/gdp/kernel/char/*
#add wave  -divider VRAM
add wave -group {VRAM}  -radix hex /dut/GDPHS/gdp/vram/*
add wave  -divider Sound
#add wave  -divider SOUND
add wave -group {Sound}  -radix hex /dut/GDPHS/impl_sound/sound_inst/*
#add wave  -divider SOUND-WAVE
add wave -group {Sound_Wave}  -radix hex /dut/GDPHS/impl_sound/sound_inst/i_psg_wave/*
add wave  -divider
#add wave  -divider Timer1
add wave -group {Timer1}  -radix hex /dut/GDPHS/impl_t1/t1/*
#add wave  -divider EMU-TOP
#add wave  -radix hex /dut/floppy/floemu/*
#add wave  -divider EMU-CC
#add wave  -radix hex /dut/floppy/floemu/cc/*
#add wave  -divider EMU-CC-CG
#add wave  -radix hex /dut/floppy/floemu/cc/cg/*
#add wave  -divider EMU-CC-HDC
#add wave  -radix hex /dut/floppy/floemu/cc/hdc/*
#add wave  -divider EMU-CC-PL
#add wave  -radix hex /dut/floppy/floemu/cc/pl/*
#add wave  -divider EMU-THM
#add wave  -radix hex /dut/floppy/floemu/thm/*
#TreeUpdate [SetDefaultTree]
#configure wave -signalnamewidth 1
