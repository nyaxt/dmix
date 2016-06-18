`default_nettype none
`timescale 1ns / 1ps

module nkmd_progrom(
    input clk,

    output [31:0] data_o,
    input [31:0] addr_i);

reg [31:0] data_ff;

always @(posedge clk) begin
    case (addr_i)
// ArithInsn{M[---] d=i alu=OpAdd s=c0 memrs=MNone t=0xf100=61696 memrt=MNone}
16'h0000: data_ff <= 32'h0901f100;
// ArithInsn{M[--R] d=a alu=OpAdd s=c0 memrs=MNone t=0xf000=61440 memrt=MR}
16'h0001: data_ff <= 32'h1101f000;
// ArithInsn{M[--R] d=b alu=OpAdd s=c0 memrs=MNone t=0xf000=61440 memrt=MR}
16'h0002: data_ff <= 32'h1201f000;
// ArithInsn{M[--R] d=c alu=OpAdd s=c0 memrs=MNone t=0xf000=61440 memrt=MR}
16'h0003: data_ff <= 32'h1301f000;
// ArithInsn{M[--R] d=d alu=OpAdd s=c0 memrs=MNone t=0xf000=61440 memrt=MR}
16'h0004: data_ff <= 32'h1401f000;
// ArithInsn{M[R--] d=i alu=OpAdd s=c0 memrs=MNone t=a memrt=MNone}
16'h0005: data_ff <= 32'h29000100;
// ArithInsn{M[R--] d=i alu=OpAdd s=c0 memrs=MNone t=b memrt=MNone}
16'h0006: data_ff <= 32'h29000200;
// ArithInsn{M[R--] d=i alu=OpAdd s=c0 memrs=MNone t=c memrt=MNone}
16'h0007: data_ff <= 32'h29000300;
// ArithInsn{M[R--] d=i alu=OpAdd s=c0 memrs=MNone t=d memrt=MNone}
16'h0008: data_ff <= 32'h29000400;
// CntlFInsn{rd=c0, imm=0x01=1}
16'h0009: data_ff <= 32'h80010001;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000a: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000b: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000c: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000d: data_ff <= 32'h00000000;

// CntlFInsn{rd=c0, imm=0x00=0}
default:  data_ff <= 32'h80010001;
    endcase
end

assign data_o = data_ff;

endmodule
