`include "../nkmm_const.v"

`timescale 1ns / 1ps
// `define NODUMP

module nkmm_arch_t;

// ins
reg clk;
reg rst;

parameter TCLK = 20.0; // 50MHz
always #(TCLK/2) clk = ~clk;

initial begin
`ifndef NODUMP
    $dumpfile("nkmm_arch_t.lxt");
    $dumpvars(0, nkmm_arch_t);
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

nkmm_arch uut(
    .clk(clk), .rst(rst),
    .uart_rx(1'b1),
    .switch(8'hab)
    );

endmodule
