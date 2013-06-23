`timescale 1ns / 1ps

module resample_pipeline_t;

// ins
reg clk;

resample_pipeline uut(
    .clk(clk));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
	$dumpfile("resample_pipeline_t.lxt");
	$dumpvars(0, resample_pipeline_t);
	
	clk = 1'b0;

	$finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
