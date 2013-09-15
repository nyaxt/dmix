`timescale 1ns / 1ps

module conv_pulse_t;

// ins
reg clk_i;
reg clk_o;

reg pulse_i;

conv_pulse uut(.clk_i(clk_i), .clk_o(clk_o), .pulse_i(pulse_i));

parameter TCLK_I = 10;
parameter TCLK_O = 40;

always #(TCLK_I/2) clk_i = ~clk_i;
always #(TCLK_O/2) clk_o = ~clk_o;

initial begin
	$dumpfile("posedge_latch_t.lxt");
	$dumpvars(0, conv_pulse_t);
	
	clk_i = 0;
	clk_o = 0;
    pulse_i = 0;

    #(TCLK_I);
    pulse_i = 1;
    #(TCLK_I);
    pulse_i = 0;
    #(TCLK_I*10);

    pulse_i = 1;
    #(TCLK_I);
    #(TCLK_I);
    #(TCLK_I);
    pulse_i = 0;
    #(TCLK_I);
    #(TCLK_I);
    #(TCLK_I);
    #(TCLK_I);
    #(TCLK_I);
    #(TCLK_I);

	$finish(2);
end

endmodule
