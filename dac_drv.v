`timescale 1ns / 1ps
`define DEBUG

module dac_drv(
	input clk,
	input rst,

	output bck_o,
	output data_o,
	output lrck_o,

    input [1:0] ack_i,
	input [23:0] data_i,
    output [1:0] pop_o
	);

// 256fs * 192kHz = 49.152Mhz
reg [7:0] clk_counter;
always @(posedge clk)
	if(rst)
		clk_counter <= 0;
	else
		clk_counter <= clk_counter + 1;

// generate bck = 4x clk = 64fs * 192kHz = 12.28Mhz
assign bck_o = clk_counter[1];

// generate lrck = 256x clk = 192kHz
assign lrck_o = ~clk_counter[7];

// generate data
reg [23:0] data_i_ff [1:0];
always @(posedge clk) begin
    if(rst) begin
        data_i_ff[0] <= 0;
        data_i_ff[1] <= 0;
    end else if(ack_i[0])
        data_i_ff[0] <= data_i;
    else if (ack_i[1])
        data_i_ff[1] <= data_i;
end
assign pop_o[1] = (clk_counter == 8'b00000000);
assign pop_o[0] = (clk_counter == 8'b10000000);

reg [31:0] data_o_ff;
assign data_o = data_o_ff[31];

wire chsel = clk_counter[7];
always @(posedge clk) begin
    if(clk_counter[6:0] == 7'h7f) begin
    `ifdef DEBUG
        $display("dac_drv:   lr %d send %h", lrck_o, data_i_ff[chsel]);
    `endif
        data_o_ff <= {8'b0, data_i_ff[chsel]};
    end else if(clk_counter[1:0] == 2'b11) begin
        data_o_ff <= {data_o_ff[30:0], 1'b0};
    end
end

endmodule
