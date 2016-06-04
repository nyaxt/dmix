const ASIN = 0xf000;
const ASOUT = 0xf100;

start:
	a = R[ASIN];
	b = R[ASIN];
	c = R[ASIN];
	d = R[ASIN];
	R[ASOUT] = a;
	R[ASOUT] = b;
	R[ASOUT] = c;
	R[ASOUT] = d;
	jmp start;
