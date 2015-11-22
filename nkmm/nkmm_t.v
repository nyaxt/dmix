`include "nkmm_const.v"

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
    0: cpu_prog_data_ff <= 0;
    1: cpu_prog_data_ff <= 0;

    default:
        cpu_prog_data_ff <= 32'hZZZZ;
    endcase
end

endmodule
