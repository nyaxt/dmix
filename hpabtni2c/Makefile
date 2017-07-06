CC=sdcc
CFLAGS=--std-sdcc99 -lstm8 -mstm8 -DSTM8S103 -I./stm8s-sdcc/inc -I.
STM8FLASH=$(HOME)/github.com/vdudouyt/stm8flash/stm8flash

flash: main.ihx
	$(STM8FLASH) -c stlinkv2 -p stm8s103?3 -w main.ihx

main.ihx: rotencled.rel stm8s_gpio.rel stm8s_clk.rel stm8s_uart1.rel stm8s_i2c.rel stm8s_spi.rel stm8s_tim4.rel
	$(CC) $(CFLAGS) -o $@ $^

stm8s_i2c.rel: stm8s-sdcc/src/stm8s_i2c.c
	$(CC) $(CFLAGS) -c -o $@ $<

stm8s_tim4.rel: stm8s-sdcc/src/stm8s_tim4.c
	$(CC) $(CFLAGS) -c -o $@ $<

stm8s_spi.rel: stm8s-sdcc/src/stm8s_spi.c
	$(CC) $(CFLAGS) -c -o $@ $<

stm8s_gpio.rel: stm8s-sdcc/src/stm8s_gpio.c
	$(CC) $(CFLAGS) -c -o $@ $<

stm8s_uart1.rel: stm8s-sdcc/src/stm8s_uart1.c
	$(CC) $(CFLAGS) -c -o $@ $<

stm8s_clk.rel: stm8s-sdcc/src/stm8s_clk.c
	$(CC) $(CFLAGS) -c -o $@ $<

.c.rel:
	$(CC) -c $(CFLAGS) $<

clean:
	rm -f *.rel *.ihx *.asm *.lst *.map *.rst *.lk *.sym *.cdb

.SUFFIXES: .rel .c
.PHONY: clean flash
