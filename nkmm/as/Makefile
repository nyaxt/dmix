.PHONY: clean

plus: nkmmas plus.S
	./nkmmas < plus.S

test: nkmmas 
	./nkmmas < hoge.S

nkmmas: *.hs
	ghc nkmmas.hs

clean:
	rm -rf *.hi *.o nkmmas
