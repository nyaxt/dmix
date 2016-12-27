`timescale 1ns / 1ps
`define DEBUG

// Output Left-Justified 24bit 192kHz 2ch (Stereo) Audio Stream
module dac_drv(
    input wire clk,
    input wire rst,

    output wire bck_o,
    output wire data_o,
    output wire lrck_o,

    input wire [1:0] ack_i,
	input wire [23:0] data_i,
    output wire [1:0] pop_o);

// 256fs * 192kHz = 49.152Mhz
reg [7:0] clk_counter;
always @(posedge clk)
	if(rst)
		clk_counter <= 0;
	else
		clk_counter <= clk_counter + 1;

// generate bck = 4x clk = 64fs * 192kHz = 12.28Mhz
wire bck_int_o = clk_counter[1];
reg bck_ff;
always @(posedge clk)
	bck_ff <= bck_int_o;
assign bck_o = bck_ff;

// generate lrck = 256x clk = 192kHz
wire lrck_int_o = ~clk_counter[7];
reg lrck_ff;
always @(posedge clk)
	lrck_ff <= lrck_int_o;
assign lrck_o = lrck_ff;

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
wire data_int_o = data_o_ff[31];
reg data_o_buf_ff;
always @(posedge clk)
	data_o_buf_ff <= data_int_o;
assign data_o = data_o_buf_ff;

wire chsel = clk_counter[7];
always @(posedge clk) begin
    if(clk_counter[6:0] == 7'h7f) begin
    `ifdef DEBUG
        $display("dac_drv:   lr %d send %h", lrck_o, data_i_ff[chsel]);
    `endif
        data_o_ff <= {data_i_ff[chsel], 8'b0};
    end else if(clk_counter[1:0] == 2'b11) begin
        data_o_ff <= {data_o_ff[30:0], 1'b0};
    end
end

endmodule
