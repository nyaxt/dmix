//`define SKIP_MIXER

module dmix_top #(
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_CH = 2,
    parameter NUM_CH_LOG2 = 1,

    parameter NUM_RATE = 5,
    parameter VOL_WIDTH = 32
)(
    input clk245760_pad,
    input rst,

    input [0:(NUM_SPDIF_IN-1)] spdif_i,
    
    // I2S to DAC
    output dac_sck_o,
    output dac_bck_o,
    output dac_lrck_o,
    output dac_data_o,

    /*
    // SPI config
    input spi_cfg_sck,
    input spi_cfg_mosi,
    output spi_cfg_miso,
    input spi_cfg_ss,
    */

    /*
    // SPI peek
    input spi_peek_sck,
    output spi_peek_mosi,
    input spi_peek_miso,
    input spi_peek_ss,
    */

    // debug
    output led_o, // T3
    output [3:0] debug_o
    );

wire clk245760;
wire clk491520;
wire clk983040;
dmix_dcm dcm(
    .clk245760_pad(clk245760_pad),
    .clk245760(clk245760),
    .clk491520(clk491520),
    .clk983040(clk983040));

wire rst_ip;
wire rst_dcm;

reg [19:0] rst_counter;
always @(posedge clk245760)
    if(rst)
        rst_counter <= 0;
    else if(rst_counter != 20'hfffff)
        rst_counter <= rst_counter + 1;
assign rst_dcm = (rst_counter[19:3] == 17'h00000);
assign rst_ip = (rst_counter[19:3] == 17'h0000e);

wire [(NUM_CH*NUM_RATE-1):0] rate;
wire [(NUM_CH-1):0] rst_ch;
wire [(NUM_CH-1):0] fifo_ack;
wire [(NUM_CH*24-1):0] fifo_data;

genvar ig;
generate
for(ig = 0; ig < NUM_SPDIF_IN; ig = ig + 1) begin:g
    wire [23:0] dai_data_983040;
    wire dai_lrck_983040;
    wire dai_ack_983040;
    wire dai_locked;

    wire [(NUM_RATE-1):0] dai_rate;
    wire [191:0] dai_udata;
    wire [191:0] dai_cdata;
    spdif_dai_varclk dai(
        .clk(clk983040),
        .rst(rst_ip),
        .signal_i(spdif_i[ig]),

        .data_o(dai_data_983040),
        .ack_o(dai_ack_983040),
        // .rst_o(dai_rst_983040),
        .lrck_o(dai_lrck_983040),

        .locked_o(dai_locked),

        .udata_o(dai_udata),
        .cdata_o(dai_cdata),
        .rate_o(dai_rate));
        
    wire [23:0] dai_data_491520;
    wire dai_lrck_491520;

    reg fifo_pop_ff;
    wire fifo_empty;
    async_fifo #(.DATA_WIDTH(24 + 1)) fifo(
        .wclk(clk983040),
        .wrst(rst_ip),
        .data_i({dai_data_983040, dai_lrck_983040}),
        .ack_i(dai_ack_983040),
        // NC: .full_o

        .rclk(clk491520),
        .rrst(rst_ip),
        .data_o({dai_data_491520, dai_lrck_491520}),
        .pop_i(fifo_pop_ff), 
        .empty_o(fifo_empty));

    always @(posedge clk491520) begin
        if (rst_ip)
            fifo_pop_ff <= 0;
        else
            fifo_pop_ff <= fifo_empty ? 0 : 1;
    end

    assign rate[(ig*NUM_RATE*2) +: (NUM_RATE*2)] = {2{dai_rate}};
    assign rst_ch[(ig*2) +: 2] = {2{~dai_locked}};
    assign fifo_ack[(ig*2) +: 2] = {fifo_pop_ff & ~dai_lrck_491520, fifo_pop_ff & dai_lrck_491520};
    assign fifo_data[(ig*24*2) +: (24*2)] = {2{dai_data_491520}};
end
endgenerate

wire [(NUM_CH-1):0] resampler_pop;
wire [47:0] resampler_data;
wire [(NUM_CH-1):0] resampler_ack;

resample_pipeline #(.NUM_CH(NUM_CH), .NUM_CH_LOG2(NUM_CH_LOG2)) resampler(
    .clk(clk491520),
    .rst(rst_ip),
    .rst_ch(rst_ch),

    .rate_i(rate),
    .ack_i(fifo_ack),
    .data_i(fifo_data),
    // .pop_o(fifo_pop) NC???

    .pop_i(resampler_pop),
    .data_o(resampler_data),
    .ack_o(resampler_ack));

`ifndef SKIP_MIXER
wire [1:0] mixer_ack;
wire [23:0] mixer_data;
wire [1:0] mixer_pop;

wire [(NUM_CH*VOL_WIDTH-1):0] vol = {NUM_CH{32'h0100_0000}};

mixer #(.NUM_CH_IN(NUM_CH), .NUM_CH_IN_LOG2(NUM_CH_LOG2),
.NUM_CH_OUT(2), .NUM_CH_OUT_LOG2(1), .VOL_WIDTH(32)) mixer(
    .clk(clk491520), .rst(rst_ip), .rst_ch(rst_ch),
    .pop_o(resampler_pop), .ack_i(resampler_ack), .data_i(resampler_data),
    .vol_i(vol),
    .pop_i(mixer_pop), .data_o(mixer_data), .ack_o(mixer_ack));

dac_drv dac_drv(
    .clk(clk491520),
    .rst(rst_ip),

    .bck_o(dac_bck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_data_o),

    .ack_i(mixer_ack),
    .data_i(mixer_data),
    .pop_o(mixer_pop));
`else
wire [23:0] resampler_data_sel = resampler_ack[1] ? resampler_data[47:24] : resampler_data[23:0];

dac_drv dac_drv(
    .clk(clk491520),
    .rst(rst_ip),

    .bck_o(dac_bck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_data_o),

    .ack_i(resampler_ack),
    .data_i(resampler_data_sel),
    .pop_o(resampler_pop));
`endif

assign dac_sck_o = clk245760;//_pad;

assign led_o = g[0].dai_locked;

assign debug_o[0] = dac_sck_o;
assign debug_o[1] = dac_lrck_o;
assign debug_o[2] = dac_data_o;
assign debug_o[3] = dac_bck_o;

endmodule
