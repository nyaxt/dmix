const ASIN = 0xf100;
const ASOUT = 0xf180;

	i = ASOUT;
start:
	a = R[ASIN];
	b = R[ASIN];
	c = R[ASIN];
	d = R[ASIN];
	R[i] = a;
	R[i] = b;
	R[i] = c;
	R[i] = d;
	jmp start;
	nop;
	nop;
	nop;
	nop;
