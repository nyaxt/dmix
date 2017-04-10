LIBUSB_PREFIX:=$(shell brew --prefix libusb)
GFLAGS_PREFIX:=$(shell brew --prefix gflags)
LDFLAGS:=-L$(LIBUSB_PREFIX)/lib -lusb-1.0 -L$(GFLAGS_PREFIX)/lib -lgflags
CFLAGS:=-O0 -g -std=c++1y -I$(LIBUSB_PREFIX)/include -I$(GFLAGS_PREFIX)/include
CXX:=clang++
CLANG_FORMAT:=clang-format

dmixlpccli: dmixlpccli.o readhex.o util.o image.o
	$(CXX) $(LDFLAGS) -o $@ $^

-include Makefile.deps

Makefile.deps:
	$(CXX) $(CFLAGS) -MM *.cpp > Makefile.deps

.cpp.o:
	$(CXX) $(CFLAGS) -c $<

clean:
	rm *.o dmixlpccli Makefile.deps

format:
	$(CLANG_FORMAT) -style=Google -i *.cpp *.h

.SUFFIXES: .o .cpp

.PHONY: clean format
