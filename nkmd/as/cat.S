start:
	a = R[0xf000];
	R[0xf100] = a;
# const ASIN = 0xf000
# const ASOUT = 0xf100
# jmp start
