`timescale 1ns / 1ps

module ringbuf_t;

// ins
reg clk;
reg rst;

reg wpulse_i;

posedge_latch uut(.clk(clk), .wpulse_i(wpulse_i));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("posedge_latch_t.lxt");
	$dumpvars(0, ringbuf_t);
	
	clk = 1'b0;
    wpulse_i = 0;

    #100;
    wpulse_i = 1;
    #100;
    wpulse_i = 0;
    #100;
    wpulse_i = 1;
    #30;
    wpulse_i = 0;
    #100;

	$finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
