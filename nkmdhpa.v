`default_nettype none
`timescale 1ns / 1ps

module nkmdhpa#(
    parameter NUM_CH = 2,
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_RATE = 5,

    parameter VOL_WIDTH = NUM_CH*32,
    parameter NKMDDBG_WIDTH = 16*8,
    parameter RATE_WIDTH = NUM_SPDIF_IN*NUM_RATE,
    parameter UDATA_WIDTH = NUM_SPDIF_IN*192,
    parameter CDATA_WIDTH = UDATA_WIDTH
)(
    input clk245760_pad,
    input rst,

    input spdif_i,

    output led_locked,

    output dac_mclk_o,
    output dac_lrck_o,
    output dac_sda_o,
    output dac_sck_o,

    input csr_sck,
    output csr_miso,
    input csr_mosi,
    input csr_ss,

    input nkmd_uart_rx,
    output nkmd_uart_tx);

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

// csr wires
// - csr <-> mixer
// wire [(VOL_WIDTH-1):0] csr_mixer_vol;
// - csr <-> DAI
wire [(RATE_WIDTH-1):0] dai_csr_rate;
wire [(UDATA_WIDTH-1):0] dai_csr_udata;
wire [(CDATA_WIDTH-1):0] dai_csr_cdata;
// - csr <-> nkmd dsp
wire csr_nkmd_rst;
wire [(NKMDDBG_WIDTH-1):0] nkmd_csr_dbgout;
wire [(NKMDDBG_WIDTH-1):0] csr_nkmd_dbgin;
wire [31:0] csr_nkmd_prog_addr;
wire [31:0] csr_nkmd_prog_data;
wire csr_nkmd_prog_ack;

csr_spi #(
    .NUM_CH(NUM_CH),
    .NUM_SPDIF_IN(NUM_SPDIF_IN))
    csr_spi(
    .clk(clk491520),
    .rst(rst_ip),

    .sck(csr_sck),
    .miso(csr_miso),
    .mosi(csr_mosi),
    .ss(csr_ss),

    // csr registers access
    // .vol_o(csr_nkmd_vol),
    .nkmd_rst_o(csr_nkmd_rst),
    .nkmd_dbgout_i(nkmd_csr_dbgout),
    .nkmd_dbgin_o(csr_nkmd_dbgin),
    .rate_i(dai_csr_rate),
    .udata_i(dai_csr_udata),
    .cdata_i(dai_csr_cdata),

    // nkmd prom
    .prom_addr_o(csr_nkmd_prog_addr),
    .prom_data_o(csr_nkmd_prog_data),
    .prom_ack_o(csr_nkmd_prog_ack));

wire [23:0] dai_data_o_983040;
wire dai_lrck_o_983040;
wire dai_ack_o_983040;
wire dai_locked_o;
assign led_locked = dai_locked_o;

wire [(NUM_RATE-1):0] dai_rate_o;
spdif_dai_varclk dai(
    .clk(clk983040),
    .rst(rst_ip),
    .signal_i(spdif_i),

    .data_o(dai_data_o_983040),
    .ack_o(dai_ack_o_983040),
    // .rst_o(dai_rst_o_983040),
    .lrck_o(dai_lrck_o_983040),

    .locked_o(dai_locked_o),

    .udata_o(dai_csr_udata),
    .cdata_o(dai_csr_cdata),
    .rate_o(dai_rate_o));
assign dai_csr_rate = dai_rate_o;

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

nkmd_arch nkmd_arch(
    .clk(clk491520),
    .rst(csr_nkmd_rst),

    .uart_rx(nkmd_uart_rx),
    .uart_tx(nkmd_uart_tx),

    // FIXME
    .dai_data_i(0),
    .dai_ack_i(0),
    // output [23:0] dai_data_o,
    .dai_pop_i(0),
    // output dai_ack_o,

`ifdef PROMW
    .prog_addr_i(csr_nkmd_prog_addr),
    .prog_data_i(csr_nkmd_prog_data),
    .prog_ack_i(csr_nkmd_prog_ack),
`endif

    .dbgout_o(nkmd_csr_dbgout),
    .dbgin_i(csr_nkmd_dbgin));

endmodule
