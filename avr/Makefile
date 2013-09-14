MCU=atmega88p

SIMAVR= -DSIMAVR -I/usr/local/include/simavr -Wl,--undefined=_mmcu,--section-start=.mmcu=0x910000 -g
COMPILE = avr-gcc -Wall -Os -I. -mmcu=$(MCU) -DF_CPU=8000000L -DMCU=\"$(MCU)\"

OBJECTS = main.o

# symbolic targets:
all:	main.hex

.c.o:
	$(COMPILE) -c $< -o $@

.S.o:
	$(COMPILE) -x assembler-with-cpp -c $< -o $@

.c.s:
	$(COMPILE) -S $< -o $@

clean:
	rm -f main.hex main.lst main.obj main.cof main.list main.map main.eep.hex main.bin *.o main.s

# file targets:
main.bin:	$(OBJECTS)
	$(COMPILE) -o main.bin $(OBJECTS)

main.hex:	main.bin
	rm -f main.hex main.eep.hex
	avr-objcopy -j .text -j .data -O ihex main.bin main.hex

bake: main.hex
	sudo avrdude -cusbasp -pm88p -U flash:w:main.hex

bakefuse:
	sudo avrdude -cusbasp -pm88p -U lfuse:w:0xE2:m

disasm:	main.bin
	avr-objdump -d main.bin

cpp:
	$(COMPILE) -E main.c

.PHONY: bake bakeh
