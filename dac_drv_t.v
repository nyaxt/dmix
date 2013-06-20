`timescale 1ns / 1ps

module dac_drv_t;

// ins
reg clk;
reg rst;

reg [23:0] data_i;
reg lrck_i;
reg ack_i;

reg [3:0] clk_scaler;
always @(posedge clk)
	clk_scaler <= clk_scaler + 1;
wire clk_s = clk_scaler[3];

// - function cfgs
wire ml;
wire md;
wire mc;
wire rstb;

dac_drv uut(
    .clk(clk), .rst(rst),
    .data_i(data_i), .lrck_i(lrck_i), .ack_i(ack_i)
);

fa1242 fa1242_cfg(
	.ml(ml), .md(md), .mc(mc), .rstb(rstb),
	.clk_s(clk_s), .rst(rst)
);

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("dac_drv_t.lxt");
	$dumpvars(0, dac_drv_t);
	
	data_i = 24'h0;
    lrck_i = 0;
    ack_i = 0;

	clk = 1'b0;
	clk_scaler = 0;

	rst = 1'b0;
	#(TCLK*6);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#TCLK;

	#(TCLK*100000);
	// #(1000_000_00);
	$finish(2);
end

always @(posedge uut.pop_o) begin
    #TCLK;
    data_i = 24'h123456;
    lrck_i = 0;
    ack_i = 1;
    #TCLK;
    data_i = 24'h789abc;
    lrck_i = 1;
    ack_i = 1;
    #TCLK;
    ack_i = 0;
    lrck_i = 0;
end

always #(TCLK/2) clk = ~clk;

endmodule
