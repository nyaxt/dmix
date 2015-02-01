`timescale 1ns / 1ps

module decimator_t;

// ins
reg clk;
reg rst;
reg [23:0] data_i;
reg ack_i;

wire [2:0] factor = 3'd3;

decimator uut(.clk(clk), .rst(rst), .data_i(data_i), .ack_i(ack_i), .factor(factor));

parameter T_CLK = 10;
always #(T_CLK/2) clk = ~clk;

initial begin
	$dumpfile("decimator_t.lxt");
	$dumpvars(0, decimator_t);

    #(3000);
	$finish(2);
end

initial begin
    clk = 0;
    rst = 0;
    ack_i = 0; 
    data_i = 23'h0;

    #(T_CLK);
    rst = 1;
    #(T_CLK);
    rst = 0;
end

always begin
    #(T_CLK*10);
    data_i = data_i + 1;
    ack_i = 1; 
    #(T_CLK);
    ack_i = 0;
end

always @(posedge clk) begin
    if (uut.ack_o)
        $display("out %h", uut.data_o);
end

endmodule
