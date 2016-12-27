const DBG = 0xc800;
const DBG2 = 0xc801;

	i = DBG;
	j = DBG2;
	d = 0xbeef;
	nop;
	nop;
	nop;
	nop;
start:
	R[i] = d;
	nop;
	nop;
	nop;
	nop;
	a = R[DBG];
	b = R[DBG2];
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
	nop;
