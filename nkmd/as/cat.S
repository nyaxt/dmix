const ASIN = 0xf000;
const ASOUT = 0xf100;

	i = c0 + ASOUT;
start:
	a = c0 + R[ASIN];
	b = c0 + R[ASIN];
	c = c0 + R[ASIN];
	d = c0 + R[ASIN];
	R[i] = c0 + a;
	R[i] = c0 + b;
	R[i] = c0 + c;
	R[i] = c0 + d;
	jmp start;
