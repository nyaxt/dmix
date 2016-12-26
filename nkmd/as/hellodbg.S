const DBG = 0xc800;
const DBG2 = 0xc801;

	i = c0 + DBG;
	j = c0 + DBG2;
	d = c0 + 0xbeef;
	nop;
	nop;
	nop;
	nop;
start:
	R[i] = c0 + d;
	nop;
	nop;
	nop;
	nop;
	a = c0 + R[DBG];
	b = c0 + R[DBG2];
	nop;
	nop;
	nop;
	nop;
	R[j] = a + b;
	jmp start;
	nop;
	nop;
	nop;
	nop;
