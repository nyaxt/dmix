`default_nettype none
`timescale 1ns / 1ps

module nkmdhpa(
    input clk245760_pad,
    input rst,

    input spdif_i,

    output led_locked,

    output dac_mclk_o,
    output dac_lrck_o,
    output dac_sda_o,
    output dac_sck_o);

wire clk245760;
wire clk491520;
wire clk983040;
dmix_dcm dcm(
    .clk245760_pad(clk245760_pad),
	 .rst_dcm(rst),
    .clk245760(clk245760),
    .clk491520(clk491520),
    .clk983040(clk983040));


`ifdef SIMULATION
assign dac_mclk_o = clk245760;
`else
ODDR2 #(
    .DDR_ALIGNMENT("NONE"),
    .INIT(1'b0),
    .SRTYPE("SYNC"))
    clkfwd (
        .Q(dac_mclk_o),
        .C0(clk245760),
        .C1(~clk245760),
        .CE(1'b1),
        .D0(1'b0),
        .D1(1'b1),
        .R(1'b0),
        .S(1'b0));
`endif

reg [4:0] rst_delay_counter;
always @(posedge clk245760) begin
    if (rst) begin
        rst_delay_counter <= 0;
    end else if(rst_delay_counter != 5'h1f) begin
        rst_delay_counter <= rst_delay_counter + 1;
    end
end

reg rst_delayed_ff;
always @(posedge clk245760) begin
    rst_delayed_ff <= rst_delay_counter == 5'h1e;
end
wire rst_ip = rst_delayed_ff;

wire [23:0] dai_data_o_983040;
wire dai_lrck_o_983040;
wire dai_ack_o_983040;
wire dai_locked_o;
assign led_locked = dai_locked_o;

wire [191:0] dai_udata_o;
wire [191:0] dai_cdata_o;
wire [4:0] dai_rate_o;

spdif_dai_varclk dai(
    .clk(clk983040),
    .rst(rst_ip),
    .signal_i(spdif_i),

    .data_o(dai_data_o_983040),
    .ack_o(dai_ack_o_983040),
    // .rst_o(dai_rst_o_983040),
    .lrck_o(dai_lrck_o_983040),

    .locked_o(dai_locked_o),

    .udata_o(dai_udata_o),
    .cdata_o(dai_cdata_o),
    .rate_o(dai_rate_o));

wire [23:0] dai_data_o_491520;
wire dai_lrck_o_491520;

reg fifo_pop_ff;
wire fifo_empty_o;
async_fifo #(.DATA_WIDTH(24 + 1)) fifo(
    .wclk(clk983040),
    .wrst(rst_ip),
    .data_i({dai_data_o_983040, dai_lrck_o_983040}),
    .ack_i(dai_ack_o_983040),
    // NC: .full_o

    .rclk(clk491520),
    .rrst(rst_ip),
    .data_o({dai_data_o_491520, dai_lrck_o_491520}),
    .pop_i(fifo_pop_ff), 
    .empty_o(fifo_empty_o));

always @(posedge clk491520) begin
    if (rst_ip)
        fifo_pop_ff <= 0;
    else
        fifo_pop_ff <= fifo_empty_o ? 0 : 1;
end

wire [1:0] resampler_dac_pop;
wire [47:0] resampler_dac_data;
wire [1:0] resampler_dac_ack;

wire [1:0] fifo_ack = {fifo_pop_ff & ~dai_lrck_o_491520, fifo_pop_ff & dai_lrck_o_491520};
resample_pipeline #(.NUM_CH(2), .NUM_CH_LOG2(1)) resampler(
    .clk(clk491520),
    .rst(rst_ip),
    .rst_ch({2{~dai_locked_o}}),

    .rate_i({2{dai_rate_o}}),
    .ack_i(fifo_ack),
    .data_i({2{dai_data_o_491520}}),
    // .pop_o(fifo_pop) NC???

    .pop_i(resampler_dac_pop),
    .data_o(resampler_dac_data),
    .ack_o(resampler_dac_ack));

wire [23:0] resampler_dac_data_sel = resampler_dac_ack[1] ? resampler_dac_data[47:24] : resampler_dac_data[23:0];
dac_drv dac_drv(
    .clk(clk491520),
    .rst(rst_ip),

    .bck_o(dac_sck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_sda_o),

    .ack_i(resampler_dac_ack),
    .data_i(resampler_dac_data_sel),
    .pop_o(resampler_dac_pop));

endmodule
