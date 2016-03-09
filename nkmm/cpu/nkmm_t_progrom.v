`include "../nkmm_const.v"
module nkmm_progrom(
    input clk,
    
    output [`INSN_WIDTH-1:0] prog_data_o,
    input [`ADDR_WIDTH-1:0] prog_addr_i);

reg [`INSN_WIDTH-1:0] data_ff;

always @(posedge clk) begin
    case (prog_addr_i)
// **** HEADER END ****
// Insn{M[--] A OpAdd(R0, imm 65)}
16'h0000: data_ff <= 32'h04010041;
// Insn{M[W-] A OpAdd(R0, imm 61440)}
16'h0001: data_ff <= 32'h4401f000;
// Insn{M[--] A OpAdd(A, imm 1)}
16'h0002: data_ff <= 32'h04110001;
// Insn{M[--] PC OpAdd(R0, imm 2)}
16'h0003: data_ff <= 32'h1c010002;
// Insn{M[--] B OpAdd(R0, imm 0)}
16'h0004: data_ff <= 32'h08010000;
// Insn{M[--] C OpAdd(R0, imm 0)}
16'h0005: data_ff <= 32'h0c010000;
// **** FOOTER BEGIN ****
// Insn{M[--] D OpAdd(R0, imm 0)}
default: data_ff <= 32'h10010000;
    endcase
end

assign prog_data_o = data_ff;

endmodule
