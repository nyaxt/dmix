TARGET=nkmd_arch
IVFLAGS=-DSIMULATION -DPROMW

dump: $(TARGET)_t.lxt
	gtkwave $(TARGET)_t.lxt &

runsim: $(TARGET)_t.lxt

.vvp.lxt:
	vvp $< -lxt2

$(TARGET)_t.vvp: nkmd_arch_t.v $(filter-out $(wildcard *_t.v), $(wildcard *.v)) ../cpu/cpu.v ../cpu/progrom_cat.v ../../uart.v
	iverilog $(IVFLAGS) -o $@ $^

.SUFFIXES: .lxt .vvp

.PHONY: clean
clean:
	rm -f $(TARGET)_t.lxt $(TARGET)_t.vvp
