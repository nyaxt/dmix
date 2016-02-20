`include "../nkmm_const.v"

module nkmm_progrom(
    input clk,
    
    output [`INSN_WIDTH-1:0] prog_data_o,
    input [`ADDR_WIDTH-1:0] prog_addr_i);

reg [`INSN_WIDTH-1:0] out_ff;

always @(posedge clk) begin
    case (prog_addr_i)
    // Insn{M[--] B OpAdd(R0, imm 2)}
    16'h0001: out_ff <= 32'h08010002;
    // Insn{M[--] C OpAdd(R0, imm 3)}
    16'h0002: out_ff <= 32'h0c010003;
    // Insn{M[--] A OpAdd(B, C)}
    16'h0003: out_ff <= 32'h04260000;
    // Insn{M[W-] A OpAdd(R0, SP)}
    16'h0004: out_ff <= 32'h440c0000;
    default:
        out_ff <= 32'h04260000;
    endcase
end

assign prog_data_o = out_ff;

endmodule
