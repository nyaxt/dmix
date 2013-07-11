module hw_test_t;

reg clk;
reg rst;

hw_test uut(.clk24576(clk), .rst(rst));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)
always #(TCLK/2) clk = ~clk;
initial begin
	$dumpfile("hw_test_t.lxt");
	$dumpvars(0, uut);

    clk = 1'b0;
	rst = 1'b0;
	#(TCLK*6);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#TCLK;

	#(TCLK*100000);
	$finish(2);
end

endmodule
