`timescale 1ns / 1ps

module dac_drv_t;

// ins
reg clk;
reg rst;

reg [23:0] data_i;
reg [1:0] ack_i;

dac_drv uut(
    .clk(clk), .rst(rst),
    .data_i(data_i), .ack_i(ack_i));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("dac_drv_t.lxt");
	$dumpvars(0, dac_drv_t);
	
	data_i = 24'h0;
    ack_i = 2'b00;

	clk = 1'b0;

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

always @(posedge uut.pop_o[0]) begin
    #TCLK;
    data_i = 24'h123456;
    ack_i[0] = 1'b1;
    #TCLK;
    ack_i[0] = 1'b0;
end

always @(posedge uut.pop_o[1]) begin
    #TCLK;
    data_i = 24'h789abc;
    ack_i[1] = 1'b1;
    #TCLK;
    ack_i[1] = 1'b0;
end

endmodule
