module dmix_top #(
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_CH = NUM_SPDIF_IN
)(
    input clk112896,
    input clk245760_pad,
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

wire rst_ip;
wire rst_dcm;

wire clk245760;
wire clk903168; // =  44.1kHz * 64 bits * 32 clk/bit = 90.3168Mhz
wire clk983040; // =  48.0kHz * 64 bits * 32 clk/bit = 98.3040Mhz
                // =  96.0kHz * 64 bits * 16 clk/bit = 98.3040Mhz
                // = 192.0kHz * 64 bits *  8 clk/bit = 98.3040Mhz
dcm_90320 dcm_90320 (
    .CLKIN_IN(clk112896),
    .USER_RST_IN(rst_dcm),
    .CLKFX_OUT(clk903168));
dcm_983040 dcm_983040 (
    .CLKIN_IN(clk245760_pad), 
    .CLKIN_IBUFG_OUT(clk245760),
    .USER_RST_IN(rst_dcm), 
    .CLKFX_OUT(clk983040));

reg [19:0] rst_counter;
always @(posedge clk245760)
    if(rst)
        rst_counter <= 0;
    else if(rst != 20'hfffff)
        rst_counter <= rst_counter + 1;
assign rst_dcm = (rst_counter[19:3] == 17'h00000);
assign rst_ip = (rst_counter[19:3] == 17'h0000e);

genvar ig;
generate
for(ig = 0; ig < NUM_SPDIF_IN; ig = ig + 1) begin:g
    wire [23:0] dai_data;
    wire dai_locked;
    wire dai_rst;
    wire dai_ack;
    wire dai_lrck;
    wire [191:0] dai_udata;
    wire [191:0] dai_cdata;
    wire [3:0] dai_rate;

    spdif_dai_varclk dai(
        .clk(clk245760),
        .rst(rst_ip),
        .signal_i(spdif_i[ig]),

        .data_o(dai_data),
        .ack_o(dai_ack),
        .rst_o(dai_rst),
        .locked_o(dai_locked),
        .lrck_o(dai_lrck),
        .udata_o(dai_udata),
        .cdata_o(dai_cdata),
    
        .rate_o(dai_rate));

    wire [1:0] resampler_ack_i = {dai_ack & dai_lrck, dai_ack & ~dai_lrck};

    wire [1:0] resampled_pop_i;
    wire [23:0] resampled_data_o;

//`define asdf
`ifdef asdf
    wire [1:0] resampled_ack_o;

    resample_pipeline resampler(
        .clk(clk245760),
        .rst(rst_ip),

        .rate_i(dai_rate),

        // data input
        // .pop_o(NOT CONNECTED),
        .data_i(dai_data),
        .ack_i(resampler_ack_i),

        // 192k output
        .pop_i(resampled_pop_i),
        .data_o(resampled_data_o),
        .ack_o(resampled_ack_o));
`else
    reg [3:0] pulse_counter;
    always @(posedge clk245760) begin
        if(dai_ack) begin
            pulse_counter <= 4'h4;
        end else if(pulse_counter > 0) begin
            pulse_counter <= pulse_counter - 1;
        end
    end
    wire wpulse_o = pulse_counter > 0;

    reg [1:0] resampled_ack_o;

    wire [23:0] datal;
    ringbuf rbl(
        .clk(clk245760),
        .rst(rst_ip),

        // data input
        .data_i(dai_data),
        .we_i(wpulse_o & dai_lrck),

        // out
        .pop_i(resampled_pop_i[0]),
        .offset_i(0),
        .data_o(datal));

    wire [23:0] datar;
    ringbuf rbr(
        .clk(clk245760),
        .rst(rst_ip),

        // data input
        .data_i(dai_data),
        .we_i(wpulse_o & ~dai_lrck),

        // out
        .pop_i(resampled_pop_i[1]),
        .offset_i(0),
        .data_o(datar));
    
    always @(posedge clk245760) begin
        resampled_ack_o <= 0;
        
        if(resampled_pop_i[0])
            resampled_ack_o[0] <= 1;
        if(resampled_pop_i[1])
            resampled_ack_o[1] <= 1;       
    end
    assign resampled_data_o = (resampled_ack_o[0] ? datal : 0) | (resampled_ack_o[1] ? datar : 0);
`endif
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

assign led_o = g[0].dai_locked;

endmodule
