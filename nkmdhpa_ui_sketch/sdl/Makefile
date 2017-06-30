CXXFLAGS:=--std=c++1y -g -O0 $(shell sdl2-config --cflags) -I../../nkmdhpa_ui
LDFLAGS:=$(shell sdl2-config --libs)

OBJS:=xorshift.o surface.o uiview.o main.o

run: nkmdhpa_ui_sketch
	./nkmdhpa_ui_sketch

format:
	clang-format -style=Google -i *.cpp

nkmdhpa_ui_sketch: $(OBJS)
	clang++ $(LDFLAGS) -o $@ $^

xorshift.o: ../../nkmdhpa_ui/xorshift.cpp
	clang++ -c $(CXXFLAGS) $<

surface.o: ../../nkmdhpa_ui/surface.cpp
	clang++ -c $(CXXFLAGS) $<

uiview.o: ../../nkmdhpa_ui/uiview.cpp
	clang++ -c $(CXXFLAGS) $<

clean:
	rm -f $(OBJS) nkmdhpa_ui_sketch

.PHONY: clean run format
