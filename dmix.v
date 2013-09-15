module dmix_top #(
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_CH = NUM_SPDIF_IN
)(
    input clk245760_pad,
    input rst,

    input [0:(NUM_SPDIF_IN-1)] spdif_i,
    
    // I2S to DAC
    output dac_sck_o,
    output dac_bck_o,
    output dac_lrck_o,
    output dac_data_o,

    // SPI config
    input spi_cfg_sck,
    input spi_cfg_mosi,
    output spi_cfg_miso,
    input spi_cfg_ss,

    /*
    // SPI peek
    input spi_peek_sck,
    output spi_peek_mosi,
    input spi_peek_miso,
    input spi_peek_ss,
    */

    // debug
    output led_o // T3
    );

wire rst_ip;
wire rst_dcm;

wire clk245760;// = clk245760_pad;
wire clk983040; // =  48.0kHz * 64 bits * 32 clk/bit = 98.3040Mhz
                // =  96.0kHz * 64 bits * 16 clk/bit = 98.3040Mhz
                // = 192.0kHz * 64 bits *  8 clk/bit = 98.3040Mhz
dcm_983040 dcm_983040 (
    .CLKIN_IN(clk245760_pad), 
    .CLKIN_IBUFG_OUT(clk245760),
    .USER_RST_IN(rst_dcm),
    .CLKFX_OUT(clk983040));

reg [19:0] rst_counter;
always @(posedge clk245760)
    if(rst)
        rst_counter <= 0;
    else if(rst_counter != 20'hfffff)
        rst_counter <= rst_counter + 1;
assign rst_dcm = (rst_counter[19:3] == 17'h00000);
assign rst_ip = (rst_counter[19:3] == 17'h0000e);

wire [(NUM_CH*2*16-1):0] vol;
wire [(NUM_CH*4-1):0] rate;
wire [(NUM_CH*192-1):0] udata;
wire [(NUM_CH*192-1):0] cdata;
csr_spi #(.NUM_CH(NUM_CH)) csr_spi(
    .clk(clk245760), .rst(rst_ip),
    .sck(spi_cfg_sck), .mosi(spi_cfg_mosi), .miso(spi_cfg_miso), .ss(spi_cfg_ss),
    .vol_o(vol), .rate_i(rate), .udata_i(udata), .cdata_i(cdata));

genvar ig;
generate
for(ig = 0; ig < NUM_SPDIF_IN; ig = ig + 1) begin:g
    wire [23:0] dai_data;
    wire dai_locked;
    wire dai_rst_983040;
    wire dai_ack_983040;
    wire dai_lrck;
    wire [3:0] dai_rate;
    wire [191:0] dai_udata;
    wire [191:0] dai_cdata;
    assign rate[(ig*4) +: 4] = dai_rate;
    assign udata[(ig*192) +: 192] = dai_udata;
    assign cdata[(ig*192) +: 192] = dai_cdata;

    spdif_dai_varclk dai(
        .clk(clk983040),
        .rst(rst_ip),
        .signal_i(spdif_i[ig]),

        .data_o(dai_data),
        .ack_o(dai_ack_983040),
        .rst_o(dai_rst_983040),
        .locked_o(dai_locked),
        .lrck_o(dai_lrck),
        .udata_o(dai_udata),
        .cdata_o(dai_cdata),
    
        .rate_o(dai_rate));
        
    wire [1:0] resampled_pop_i;
    wire [23:0] resampled_data_o;

    wire dai_ack_245760;
    wire dai_rst_245760;
    conv_pulse conv_ack(.clk_in(clk983040), .clk_out(clk245760), .pulse_in(dai_ack_983040), .pulse_out(dai_ack_245760));
    conv_pulse conv_rst(.clk_in(clk983040), .clk_out(clk245760), .pulse_in(dai_rst_983040), .pulse_out(dai_rst_245760));

    wire [1:0] resampler_ack_i = {dai_lrck & dai_ack_245760, ~dai_lrck & dai_ack_245760};
    wire [1:0] resampled_ack_o;

    resample_pipeline resampler(
        .clk(clk245760),
        .rst(dai_rst_245760),

        .rate_i(4'b0010),

        // data input
        // .pop_o(NOT CONNECTED),
        .data_i(dai_data),
        .ack_i(resampler_ack_i),

        // 192k output
        .pop_i(resampled_pop_i & 2'b01),
        .data_o(resampled_data_o),
        .ack_o(resampled_ack_o));
end
endgenerate

wire [1:0] mix_pop_i;
wire [23:0] mix_data_o;
wire [1:0] mix_ack_o;

mixer #(
    .NUM_CH(1), .NUM_CH_LOG2(1),
    .FS(128)
) mixer(
    .clk(clk245760),
    .rst(rst_ip),

    .pop_o({g[0].resampled_pop_i}),//, g[1].resampled_pop_i, g[2].resampled_pop_i}),
    .data_i({g[0].resampled_data_o}),//, g[1].resampled_data_o, g[2].resampled_data_o}),
    .vol_i(vol),
    // .ack_i is assumed to be 1clk latency to pop_o

    .pop_i(mix_pop_i),
    .data_o(mix_data_o),
    .ack_o(mix_ack_o));

dac_drv dac_drv(
    .clk(clk245760),
    .rst(rst_ip),

    .sck_o(dac_sck_o),
    .bck_o(dac_bck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_data_o),

    .ack_i(mix_ack_o),
    .data_i(mix_data_o),
    .pop_o(mix_pop_i));

assign led_o = spdif_i[0];

endmodule
