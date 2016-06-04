TARGET=cpu
PERIPHERALS=progrom_cat.v

dump: nkmd_t.lxt
	gtkwave $< &

runsim: nkmd_t.lxt

.vvp.lxt:
	vvp $< -lxt2

nkmd_t.vvp: $(TARGET)_t.v $(TARGET).v $(PERIPHERALS)
	iverilog -Wall -DSIMULATION -o $@ $^

.SUFFIXES: .lxt .vvp

.PHONY: clean
clean:
	rm -f nkmd_t.lxt nkmd_t.vvp
