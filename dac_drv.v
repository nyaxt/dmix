`timescale 1ns / 1ps

module dac_drv(
	output sck,
	output bck,
	output data,
	output lrck,
	input [23:0] l_data,
	input [23:0] r_data,
	input clk,
	input rst
	);

assign sck = clk; // 128fs * 192kHz = 24.57Mhz

wire [6:0] next_clk = clk_counter + 1;
reg [6:0] clk_counter;
always @(posedge clk or rst)
	if(rst)
		clk_counter <= 0;
	else
		clk_counter <= next_clk;

// generate bck = 2x clk = 64fs * 192kHz = 12.28Mhz
assign bck = clk_counter[0];

// generate lrck = 64x clk = 192kHz
assign lrck = clk_counter[6];

// generate data
function gen_data(
	input [23:0] l_data,
	input [23:0] r_data,
	input [5:0] bck_counter);

	reg [23:0] lr_data;
	reg [4:0] lr_data_idx;

	begin
		lr_data[23:0] = (bck_counter < 6'd32) ? l_data[23:0] : r_data[23:0];

		if(bck_counter[4:0] < 5'd24)
		begin
			lr_data_idx[4:0] = 5'd23 - bck_counter[4:0]; // msb first
			gen_data = lr_data[lr_data_idx];
		end
		else
		begin
			lr_data_idx = 5'b0;
			gen_data = 1'b0;
		end
   end
endfunction

reg data_r;
always @(posedge clk)
	data_r <= gen_data(l_data, r_data, next_clk[6:1]);

assign data = data_r;

endmodule
