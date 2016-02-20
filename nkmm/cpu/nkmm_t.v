`include "../nkmm_const.v"

`timescale 1ns / 1ps
// `define NODUMP

module nkmm_t;

// ins
reg clk;
reg rst;

reg [`ACCUM_WIDTH-1:0] cpu_data_ff;
reg [`INSN_WIDTH-1:0] cpu_prog_data_ff;
nkmm_cpu cpu(
    .clk(clk),
    .rst(rst),

    .data_i(cpu_data_ff),
    .prog_data_i(cpu_prog_data_ff)
);

parameter TCLK = 10.0; // 100MHz

initial begin
`ifndef NODUMP
    $dumpfile("nkmm_t.lxt");
    $dumpvars(0, nkmm_t);
`endif

    clk = 1'b0;

    rst = 1'b0;
    #(TCLK*6);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

`ifndef NODUMP
    #100_000;
    $finish(2);
`endif
end

always #(TCLK/2) clk = ~clk;

// prog ram
always @(posedge clk) begin
    case(cpu.prog_addr_o)
    // Insn{M[--] B OpAdd(R0, imm 2)}
    16'h0001: cpu_prog_data_ff <= 32'h08010002;
    // Insn{M[--] C OpAdd(R0, imm 3)}
    16'h0002: cpu_prog_data_ff <= 32'h0c010003;
    // Insn{M[--] A OpAdd(B, C)}
    16'h0003: cpu_prog_data_ff <= 32'h04260000;
    // Insn{M[W-] A OpAdd(R0, SP)}
    16'h0004: cpu_prog_data_ff <= 32'h440c0000;
    default:
        //cpu_prog_data_ff <= 32'hZZZZ;
        cpu_prog_data_ff <= 32'h04260000;
    endcase
end

endmodule
