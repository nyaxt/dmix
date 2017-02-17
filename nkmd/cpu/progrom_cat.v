`default_nettype none
`timescale 1ns / 1ps

module nkmd_progrom(
    input wire clk,

    output wire [31:0] data_o,
    input wire [31:0] addr_i);

reg [31:0] data_ff;

always @(posedge clk) begin
    case (addr_i)
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h0000: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h0001: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h0002: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h0003: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=a alu=OpAdd s=c0 memrs=MNone t=0x01=1 memrt=MNone}
16'h0004: data_ff <= 32'h01010001;
// ArithInsn{M[---] d=b alu=OpAdd s=c0 memrs=MNone t=0x02=2 memrt=MNone}
16'h0005: data_ff <= 32'h02010002;
// ArithInsn{M[---] d=c alu=OpAdd s=c0 memrs=MNone t=0x03=3 memrt=MNone}
16'h0006: data_ff <= 32'h03010003;
// ArithInsn{M[---] d=d alu=OpAdd s=c0 memrs=MNone t=0x04=4 memrt=MNone}
16'h0007: data_ff <= 32'h04010004;
// ArithInsn{M[---] d=e alu=OpAdd s=c0 memrs=MNone t=a memrt=MNone}
16'h0008: data_ff <= 32'h05000100;
// ArithInsn{M[---] d=f alu=OpAdd s=c0 memrs=MNone t=b memrt=MNone}
16'h0009: data_ff <= 32'h06000200;
// ArithInsn{M[---] d=g alu=OpAdd s=c0 memrs=MNone t=c memrt=MNone}
16'h000a: data_ff <= 32'h07000300;
// ArithInsn{M[---] d=h alu=OpAdd s=c0 memrs=MNone t=d memrt=MNone}
16'h000b: data_ff <= 32'h08000400;
// CntlFInsn{rd=c0, imm=0x04=4}
16'h000c: data_ff <= 32'h80010004;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000d: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000e: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h000f: data_ff <= 32'h00000000;
// ArithInsn{M[---] d=c0 alu=OpAdd s=c0 memrs=MNone t=c0 memrt=MNone}
16'h0010: data_ff <= 32'h00000000;

// CntlFInsn{rd=c0, imm=0x00=0}
default:  data_ff <= 32'h80010001;
    endcase
end

assign data_o = data_ff;

endmodule
`default_nettype wire
