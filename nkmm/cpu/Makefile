TARGET=nkmm

dump: $(TARGET)_t.lxt
	gtkwave $(TARGET)_t.lxt &

runsim: $(TARGET)_t.lxt

.vvp.lxt:
	vvp $< -lxt2

$(TARGET)_t.vvp: $(TARGET)_t.v $(TARGET).v ../nkmm_const.v
	iverilog -Wall -DSIMULATION -o $@ $^

.SUFFIXES: .lxt .vvp

.PHONY: clean
clean:
	rm $(TARGET)_t.lxt $(TARGET)_t.vvp
