`timescale 1ns / 1ps

module hw_test(
    input clk24576,
    input rst,

	output sck_o,
	output bck_o,
	output data_o,
	output lrck_o);

reg [1:0] pop_s;
wire [1:0] ack;
wire [23:0] data [1:0];
synth l(
	.clk(clk24576), .rst(rst),
    .pop_i(pop_s[0]), .ack_o(ack[0]), .data_o(data[0]));
synth r(
	.clk(clk24576), .rst(rst),
    .pop_i(pop_s[1]), .ack_o(ack[1]), .data_o(data[1]));

wire [23:0] data_d = data[0] | data[1];
wire ack_d = |ack;
reg lrck_d;
wire pop_d;
dac_drv d(
	.clk(clk24576), .rst(rst),
    .sck_o(sck_o), .bck_o(bck_o), .data_o(data_o), .lrck_o(lrck_o),
    .data_i(data_d), .lrck_i(lrck_d), .ack_i(ack_d), .pop_o(pop_d));

always @(posedge clk24576) begin
    if(rst) begin
        pop_s <= 2'b00;
        lrck_d <= 0;
    end else if(pop_d) begin
        pop_s <= 2'b01;
        lrck_d <= 0;
    end else if(pop_s[0]) begin
        pop_s <= 2'b10;
        lrck_d <= 0;
    end else begin
        pop_s <= 2'b00;
        lrck_d <= 1;
    end
end

endmodule
