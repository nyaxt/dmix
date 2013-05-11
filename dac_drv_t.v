`timescale 1ns / 1ps

module dac_drv_t;

// ins
reg [23:0] l_data;
reg [23:0] r_data;
reg clk;
reg rst;

// outs
wire sck;
wire bck;
wire data;
wire lrck;

dac_drv uut(
	.sck(sck), .bck(bck), .data(data), .lrck(lrck),
	.l_data(l_data), .r_data(r_data), .clk(clk), .rst(rst)
);

parameter TCLK = 41.0;

initial begin
	$dumpfile("dac_drv_t.lxt");
	$dumpvars(0, uut);
	
	l_data = 24'b0;
	r_data = 24'hffffff;
	clk = 1'b0;

	rst = 1'b0;
	#(TCLK*6);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#TCLK;

	#(TCLK*1024);
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
