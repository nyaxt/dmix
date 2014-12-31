`timescale 1ns / 1ps

module pop_latch_t;

// ins
reg clk;
reg rst;

reg pop_i;
reg ack_pop_i;

pop_latch uut(
    .clk(clk), .rst(rst),
    .pop_i(pop_i), .ack_pop_i(ack_pop_i));

parameter T_CLK = 10;
always #(T_CLK/2) clk = ~clk;

initial begin
	$dumpfile("pop_latch_t.lxt");
	$dumpvars(0, pop_latch_t);

    clk = 0;
    pop_i = 0;
    ack_pop_i = 0;

    rst = 0;
    #(T_CLK);
    rst = 1;
    #(T_CLK);
    rst = 0;

    pop_i = 1;
    #(T_CLK);
    pop_i = 0;
    #(T_CLK);

    ack_pop_i = 1;
    #(T_CLK);
    ack_pop_i = 0;
    #(T_CLK);

    #(T_CLK * 5);

    pop_i = 1;
    ack_pop_i = 1;
    #(T_CLK);
    pop_i = 0;
    ack_pop_i = 0;
    #(T_CLK);

    #(T_CLK * 5);
	$finish(2);
end

endmodule
