GLFW_PREFIX=$(shell brew --prefix glfw3)
LIBPNG_PREFIX=$(shell brew --prefix libpng)
LDFLAGS=-framework OpenGL -L$(GLFW_PREFIX)/lib -lglfw3 -L$(LIBPNG_PREFIX)/lib -lpng
CFLAGS=-g -std=c++1y -I$(GLFW_PREFIX)/include -I$(LIBPNG_PREFIX)/include
CXX=clang++

controller: main.o spritejson.o
	$(CXX) $(LDFLAGS) -o $@ $<

.cpp.o:
	$(CXX) $(CFLAGS) -c $<

clean:
	rm *.o controller

.SUFFIXES: .o .cpp

.PHONY: clean
