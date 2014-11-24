TARGET=nkmm

dump: $(TARGET)_t.lxt
	gtkwave $(TARGET)_t.lxt &

runsim: $(TARGET)_t.lxt

.vvp.lxt:
	vvp $< -lxt2

$(TARGET)_t.vvp: $(TARGET)_t.v $(TARGET).v nkmm_const.v
	iverilog -o $@ $^

.SUFFIXES: .lxt .vvp
