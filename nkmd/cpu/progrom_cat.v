`default_nettype none
`timescale 1ns / 1ps

module nkmd_progrom(
    input clk,

    output [31:0] data_o,
    input [31:0] addr_i);

reg [31:0] data_ff;

always @(posedge clk) begin
    case (addr_i)
// ArithInsn{M[-R] a OpAdd(c0, imm 0xf000=61440)}
16'h0000: data_ff <= 32'h1101f000;
// ArithInsn{M[-R] b OpAdd(c0, imm 0xf000=61440)}
16'h0001: data_ff <= 32'h1201f000;
// ArithInsn{M[-R] c OpAdd(c0, imm 0xf000=61440)}
16'h0002: data_ff <= 32'h1301f000;
// ArithInsn{M[-R] d OpAdd(c0, imm 0xf000=61440)}
16'h0003: data_ff <= 32'h1401f000;
// ArithInsn{M[R-] a OpAdd(c0, imm 0xf100=61696)}
16'h0004: data_ff <= 32'h5101f100;
// ArithInsn{M[R-] b OpAdd(c0, imm 0xf100=61696)}
16'h0005: data_ff <= 32'h5201f100;
// ArithInsn{M[R-] c OpAdd(c0, imm 0xf100=61696)}
16'h0006: data_ff <= 32'h5301f100;
// ArithInsn{M[R-] d OpAdd(c0, imm 0xf100=61696)}
16'h0007: data_ff <= 32'h5401f100;
// CntlFInsn{rd=c0, imm=0x00=0}
16'h0008: data_ff <= 32'h80010000;

// CntlFInsn{rd=c0, imm=0x00=0}
default:  data_ff <= 32'h80010000;
    endcase
end

assign data_o = data_ff;

endmodule
