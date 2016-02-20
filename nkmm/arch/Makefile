TARGET=nkmm_arch

dump: $(TARGET)_t.lxt
	gtkwave $(TARGET)_t.lxt &

runsim: $(TARGET)_t.lxt

.vvp.lxt:
	vvp $< -lxt2

$(TARGET)_t.vvp: *.v ../cpu/nkmm.v ../nkmm_const.v ../../uart.v
	iverilog -DSIMULATION -o $@ $^

.SUFFIXES: .lxt .vvp

.PHONY: clean
clean:
	rm $(TARGET)_t.lxt $(TARGET)_t.vvp
