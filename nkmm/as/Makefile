.PHONY: clean

test: nkmmas 
	./nkmmas < hoge.S

nkmmas: nkmmas.hs
	ghc $<

clean:
	rm -rf *.hi *.o nkmmas
