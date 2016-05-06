const ASIN = 0xf000;
const ASOUT = 0xf100;

start:
	a = R[ASIN];
	R[ASOUT] = a;
	jmp start;
