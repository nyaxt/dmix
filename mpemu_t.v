`timescale 1ns / 1ps

module mpemu_t;

// ins
reg clk;

reg signed [23:0] mpcand_i;
reg signed [15:0] mplier_i;

// outs
wire signed [23:0] mprod_o;

mpemu uut(
    .clk(clk),
    .mpcand_i(mpcand_i), .mplier_i(mplier_i),
    .mprod_o(mprod_o));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("mpemu_t.lxt");
	$dumpvars(0, mpemu_t);
	
	clk = 1'b0;

    mpcand_i = 24'h10000;
    mplier_i = 0;
    #TCLK;

    mpcand_i = 24'h10000;
    mplier_i = 2;
    #TCLK;

    mpcand_i = 24'h30000;
    mplier_i = 4;
    #TCLK;

    mpcand_i = 24'h50000;
    mplier_i = 6;
    #TCLK;

    mpcand_i = -24'h10000;
    mplier_i = 20;
    #TCLK;

    mpcand_i = -24'sh20000;
    mplier_i = 16'sh7fff;
    #TCLK;
    #TCLK;
    #TCLK;
    #TCLK;
    #TCLK;
    #TCLK;
    
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

always begin
    #TCLK;
    $display("%d", mprod_o);
end

endmodule
