`timescale 1ns / 1ps

module genpulse_t;

// ins
reg clk;
reg rst;

genpulse #(.WAVELENGTH(12), .WAVELENGTH_LOG2(4)) uut(.clk(clk), .rst(rst));
parameter T_CLK = 10;
always #(T_CLK/2) clk = ~clk;

initial begin
	$dumpfile("genpulse_t.lxt");
	$dumpvars(0, genpulse_t);

    #(3000);
	$finish(2);
end

initial begin
    clk = 0;
    rst = 0;

    #(T_CLK);
    rst = 1;
    #(T_CLK);
    rst = 0;
end

endmodule
