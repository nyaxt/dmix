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

// generate bck = 2x clk
reg bck_ff;
always @(posedge sck or rst)
	if(rst)
		bck_ff <= 0;
	else
		bck_ff <= ~bck_ff;

assign bck = bck_ff; // 64fs * 192kHz = 12.29Mhz

reg [5:0] bck_counter;
always @(posedge bck or rst)
	if(rst)
		bck_counter <= 0;
	else
		bck_counter <= bck_counter + 1;

// generate lrck = 64x bck
reg lrck_ff;
always @(posedge bck)
	lrck_ff <= ~bck_counter[5]; // 192kHz

assign lrck = lrck_ff;

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
always @(posedge bck)
	data_r <= gen_data(l_data, r_data, bck_counter);

assign data = data_r;

endmodule
