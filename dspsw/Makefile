play: out.wav
	play $<	

out.wav: out.dat ../filterpy/wavdat.py
	python ../filterpy/wavdat.py $< $@

out.dat: resample in.dat
	./resample {in,out}.dat

in.dat: ../filterpy/damashie.wav ../filterpy/wavdat.py
	python ../filterpy/wavdat.py $< $@

resample: resample.c filter.h
	cc -g $< -o $@

filter.h:
	(cd ../filterpy; python genfir.py)

clean:
	rm -rf filter.h resmaple *.dat resample.dSYM

.PHONY: play clean
