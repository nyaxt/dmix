module dmix_top #(
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_CH = NUM_SPDIF_IN
)(
    input clk112896,
    input clk245760,
    input rst,

    input [0:(NUM_SPDIF_IN-1)] spdif_i,
    
    // I2S to DAC
    output dac_sck_o,
    output dac_bck_o,
    output dac_lrck_o,
    output dac_data_o,

    // SPI config
    /*
    input spi_cfg_sclk,
    input spi_cfg_mosi,
    input spi_cfg_miso,
    */

    /*
    // SPI peek
    input spi_peek_sclk,
    input spi_peek_mosi,
    input spi_peek_miso,
    */

    // debug
    output led_o // T3
    );

assign led_o = spdif_i[0];

wire clk903200; // =  44.1kHz * 64 bits * 32 clk/bit = 90.320Mhz
wire clk983040; // =  48.0kHz * 64 bits * 32 clk/bit = 98.304Mhz
                // =  96.0kHz * 64 bits * 16 clk/bit = 98.304Mhz
                // = 192.0kHz * 64 bits *  8 clk/bit = 98.304Mhz

genvar ig;
generate
for(ig = 0; ig < NUM_SPDIF_IN; ig = ig + 1) begin:g
    wire [23:0] dai_data;
    wire dai_ack;
    wire dai_locked;
    wire dai_lrck;
    wire [191:0] dai_udata;
    wire [191:0] dai_cdata;
    wire [3:0] dai_rate;

    spdif_dai_varclk dai(
        .clk903200(clk903200), .clk983040(clk983040),
        .rst(rst),
        .signal_i(spdif_i[ig]),

        .data_o(dai_data),
        .wpulse_o(dai_wpulse),
        .locked_o(dai_locked),
        .lrck_o(dai_lrck),
        .udata_o(dai_udata),
        .cdata_o(dai_cdata),
    
        .rate_o(dai_rate));

    wire latch_ack_o;
    posedge_latch latch(
        .clk(clk245760),
        .wpulse_i(dai_wpulse),
        .ack_o(latch_ack_o));

    wire [1:0] resampler_ack_i = {latch_ack_o & dai_lrck, latch_ack_o & ~dai_lrck};

    wire [1:0] resampled_pop_i;
    wire [23:0] resampled_data_o;
    wire [1:0] resampled_ack_o;

    resample_pipeline resampler(
        .clk(clk245760),
        .rst(rst),

        .rate_i(dai_rate),

        // data input
        // .pop_o(NOT CONNECTED),
        .data_i(dai_data),
        .ack_i(resampler_ack_i),

        // 192k output
        .pop_i(resampled_pop_i),
        .data_o(resampled_data_o),
        .ack_o(resampled_ack_o));
end
endgenerate

wire [(NUM_CH*2*16-1):0] vol = {2{16'h00ff}};
wire [1:0] mix_pop_i;
wire [23:0] mix_data_o;
wire [1:0] mix_ack_o;

mixer #(
    .NUM_CH(1), .NUM_CH_LOG2(1),
    .FS(128)
) mixer(
    .clk(clk245760),
    .rst(rst),

    .pop_o({g[0].resampled_pop_i}),//, g[1].resampled_pop_i, g[2].resampled_pop_i}),
    .data_i({g[0].resampled_data_o}),//, g[1].resampled_data_o, g[2].resampled_data_o}),
    .vol_i(vol),
    // .ack_i is assumed to be 1clk latency to pop_o

    .pop_i(mix_pop_i),
    .data_o(mix_data_o),
    .ack_o(mix_ack_o));

dac_drv dac_drv(
    .clk(clk245760),
    .rst(rst),

    .sck_o(dac_sck_o),
    .bck_o(dac_bck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_data_o),

    .ack_i(mix_ack_o),
    .data_i(mix_data_o),
    .pop_o(mix_pop_i));

endmodule
