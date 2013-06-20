`timescale 1ns / 1ps

module ringbuf_t;

// ins
reg clk;
reg rst;

reg [23:0] data_i;
reg we_i;

reg pop_i;
reg [3:0] offset_i;

ringbuf uut(
    .clk(clk), .rst(rst),
    .data_i(data_i), .we_i(we_i),
    .pop_i(pop_i), .offset_i(offset_i));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("ringbuf_t.lxt");
	$dumpvars(0, ringbuf_t);
	
	clk = 1'b0;

	data_i = 24'h0;
    we_i = 0;
    pop_i = 0;
    offset_i = 0;

	rst = 1'b0;
	#(TCLK*6);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#TCLK;

    we_i = 1;
    data_i = 24'h0;
    #TCLK;
    data_i = 24'h1;
    #TCLK;
    data_i = 24'h2;
    #TCLK;
    data_i = 24'h3;
    #TCLK;
    data_i = 24'h4;
    #TCLK;
    data_i = 24'h5;
    #TCLK;
    data_i = 24'h6;
    #TCLK;
    data_i = 24'h7;
    #TCLK;
    data_i = 24'h8;
    #TCLK;
    data_i = 24'h9;
    #TCLK;
    data_i = 24'ha;
    #TCLK;
    data_i = 24'hb;
    #TCLK;
    data_i = 24'hc;
    #TCLK;
    data_i = 24'hd;
    #TCLK;
    data_i = 24'he;
    #TCLK;
    data_i = 24'hf;
    #TCLK;
    we_i = 0;
    #TCLK;

    offset_i = 0;
    #TCLK;
    $display("15 = %d", uut.data_o);
    offset_i = 1;
    #TCLK;
    $display("14 = %d", uut.data_o);
    offset_i = 2;
    #TCLK;
    $display("13 = %d", uut.data_o);
    offset_i = 3;
    #TCLK;
    $display("12 = %d", uut.data_o);

    pop_i = 1;
    #TCLK;
    pop_i = 0;

    offset_i = 0;
    #TCLK;
    $display("0 = %d", uut.data_o);
    offset_i = 1;
    #TCLK;
    $display("15 = %d", uut.data_o);
    offset_i = 2;
    #TCLK;
    $display("14 = %d", uut.data_o);
    offset_i = 3;
    #TCLK;
    $display("13 = %d", uut.data_o);

	$finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
