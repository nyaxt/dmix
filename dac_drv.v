`timescale 1ns / 1ps

module dac_drv(
	input clk,
	input rst,

	output sck_o,
	output bck_o,
	output data_o,
	output lrck_o,

	input [23:0] data_i,
    input lrck_i,
    input ack_i,
    output pop_o
	);

assign sck_o = clk; // 128fs * 192kHz = 24.57Mhz

reg [6:0] clk_counter;
always @(posedge clk)
	if(rst)
		clk_counter <= 0;
	else
		clk_counter <= clk_counter + 1;

// generate bck = 2x clk = 64fs * 192kHz = 12.28Mhz
assign bck_o = clk_counter[0];
wire [4:0] bck_counter = clk_counter[5:1];

// generate lrck = 64x clk = 192kHz
assign lrck_o = ~clk_counter[6];

// generate data
reg [23:0] data_i_ff [1:0];
always @(posedge clk) begin
    if(rst) begin
        data_i_ff[0] <= 0;
        data_i_ff[1] <= 0;
    end else if(ack_i) begin
        data_i_ff[lrck_i] <= data_i;
    end
end

reg [31:0] data_o_ff;
assign data_o = data_o_ff[31];

wire chsel = clk_counter[6];
always @(posedge clk) begin
    if(clk_counter[5:0] == 6'h3f)
        data_o_ff <= {8'b0, data_i_ff[chsel]};
    else if(clk_counter[0] == 1'b1)
        data_o_ff <= {data_o_ff[30:0], 1'b0};
end

assign pop_o = (clk_counter == 64);

endmodule
