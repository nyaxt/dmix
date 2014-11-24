`define ACCUM_WIDTH 24
`define PROG_WIDTH 32
`define ADDR_WIDTH 16
`define OPSEL_WIDTH 4
`define REGSEL_WIDTH 3
`define IMM_WIDTH 16

// FIXME: optimize table here
// FIXME: implement carry 
`define OP_ADD 3'b000
`define OP_SUB 3'b001 
`define OP_OR  3'b010
`define OP_AND 3'b011
`define OP_XOR 3'b100
`define OP_NOT 3'b101
`define OP_SHI 3'b110

`define SHI_1 2'b00
`define SHI_2 2'b01
`define SHI_4 2'b10
`define SHI_8 2'b11
