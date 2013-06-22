`timescale 1ns / 1ps

module ringbuf_t;

// ins
reg clk;
reg rst;

reg [23:0] data_i;
reg wpulse_i;

reg pop_i;
reg [3:0] offset_i;

ringbuf uut(
    .clk(clk), .rst(rst),
    .data_i(data_i), .wpulse_i(wpulse_i),
    .pop_i(pop_i), .offset_i(offset_i));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

task recv_data;
    input [23:0] b;
    begin
        data_i = b;
        #10;
        wpulse_i = 1;
        #130;
        wpulse_i = 0;
        #500;
    end
endtask

initial begin
	$dumpfile("ringbuf_t.lxt");
	$dumpvars(0, ringbuf_t);
	
	clk = 1'b0;

	data_i = 24'h0;
    wpulse_i = 0;
    pop_i = 0;
    offset_i = 0;

	rst = 1'b0;
	#(TCLK*6);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#TCLK;

    recv_data(24'h0);
    recv_data(24'h1);
    recv_data(24'h2);
    recv_data(24'h3);
    recv_data(24'h4);
    recv_data(24'h5);
    recv_data(24'h6);
    recv_data(24'h7);
    recv_data(24'h8);
    recv_data(24'h9);
    recv_data(24'ha);
    recv_data(24'hb);
    recv_data(24'hc);
    recv_data(24'hd);
    recv_data(24'he);
    recv_data(24'hf);

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
