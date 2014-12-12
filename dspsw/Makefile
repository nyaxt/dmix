resample: resample.c filter.h
	cc $< -o $@

filter.h:
	(cd ../filterpy; python genfir.py)

clean:
	rm -f filter.h resmaple

.PHONY: clean
