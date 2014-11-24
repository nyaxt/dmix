`timescale 1ns / 1ps
// `define NODUMP

module nkmm_t;

// ins
reg clk;
reg rst;

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
    #5_000_000;
    $finish(2);
`endif
end

always #(TCLK/2) clk = ~clk;

endmodule
