.PHONY: clean

cat: nkmmas cat.S
	./nkmmas < cat.S

nkmmas: *.hs
	ghc nkmmas.hs

clean:
	rm -rf *.hi *.o nkmmas
