`timescale 1ns / 1ps

module dac_drv_t;

// ins
reg clk;
reg rst;

wire [23:0] l_data;
reg [23:0] r_data;

reg [3:0] clk_scaler;
always @(posedge clk)
	clk_scaler <= clk_scaler + 1;
wire clk_s = clk_scaler[3];

// outs
wire sck;
wire bck;
wire data;
wire lrck;

// - function cfgs
wire ml;
wire md;
wire mc;
wire rstb;

dac_drv uut(
	.sck(sck), .bck(bck), .data(data), .lrck(lrck),
	.l_data(l_data), .r_data(r_data), .clk(clk), .rst(rst)
);

synth synth(
	.data(l_data),
	.clk(clk)
);

fa1242 fa1242_cfg(
	.ml(ml), .md(md), .mc(mc), .rstb(rstb),
	.clk_s(clk_s), .rst(rst)
);

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("dac_drv_t.lxt");
	$dumpvars(0, dac_drv_t);
	
	// l_data = 24'b0;
	r_data = 24'hffffff;
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

always #(TCLK/2) clk = ~clk;

endmodule
