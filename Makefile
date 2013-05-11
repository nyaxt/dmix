dump: dac_drv_t.lxt
	gtkwave dac_drv_t.lxt &

runsim: dac_drv_t.lxt

.vvp.lxt:
	vvp $< -lxt2

dac_drv_t.vvp: fa1242.v dac_drv.v synth.v dac_drv_t.v
	iverilog -o $@ $^

.SUFFIXES: .lxt .vvp
