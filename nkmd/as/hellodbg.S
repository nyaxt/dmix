const DBG = 0xc800;
const DBG2 = 0xc801;

	i = c0 + DBG;
	j = c0 + DBG2;
	d = c0 + 0xbeef;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
start:
	R[i] = c0 + d;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
	a = c0 + R[DBG];
	b = c0 + R[DBG2];
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
	R[j] = a + b;
	jmp start;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
	c0 = c0 + c0;
