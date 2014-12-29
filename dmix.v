//`define SKIP_RESAMPLER

module dmix_top #(
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_CH = 2,
    parameter NUM_CH_LOG2 = 1
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

wire [(NUM_CH-1):0] fifo_ack;
wire [(NUM_CH*24-1):0] fifo_data;

genvar ig;
generate
for(ig = 0; ig < NUM_SPDIF_IN; ig = ig + 1) begin:g
    wire [23:0] dai_data_983040;
    wire dai_lrck_983040;
    wire dai_ack_983040;

    wire dai_locked;

    wire [3:0] dai_rate;
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

 //`define AAA
`ifdef AAA
    wire dai_ack_491520;
    conv_pulse conv_ack(.clk_i(clk983040), .clk_o(clk491520), .pulse_i(dai_ack_983040), .pulse_o(dai_ack_491520));
    assign fifo_ack[(ig*2) +: 2] = {1'b0, dai_ack_491520 & dai_lrck_983040};
    assign fifo_data[(ig*24*2) +: (24*2)] = {2{dai_data_983040}};
`else
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

    assign fifo_ack[(ig*2) +: 2] = {fifo_pop_ff & ~dai_lrck_491520, fifo_pop_ff & dai_lrck_491520};
    assign fifo_data[(ig*24*2) +: (24*2)] = {2{dai_data_491520}};
`endif
end
endgenerate

wire [11:0] bank_addr;
wire [23:0] bank_data;
rom_firbank_441_480 bank(.clk(clk491520), .addr(bank_addr), .data(bank_data));

`ifndef SKIP_RESAMPLER
wire [(NUM_CH-1):0] resampler_pop_i;
wire [23:0] resampler_data_o;
wire [(NUM_CH-1):0] resampler_ack_o;

ringbuffered_resampler #(.NUM_CH(NUM_CH), .NUM_CH_LOG2(NUM_CH_LOG2)) resampler(
    .clk(clk491520),
    .rst(rst_ip),

    .bank_addr_o(bank_addr),
    .bank_data_i(bank_data),

    .ack_i(fifo_ack),
    .data_i(fifo_data),

    .pop_i(resampler_pop_i),
    .data_o(resampler_data_o),
    .ack_o(resampler_ack_o)
    );

dac_drv dac_drv(
    .clk(clk491520),
    .rst(rst_ip),

    .bck_o(dac_bck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_data_o),

    .ack_i(resampler_ack_o),
    .data_i(resampler_data_o),
    .pop_o(resampler_pop_i));
`else

wire dac_pop_o;
wire [23:0] rb_data_o;
ringbuf rb(
    .clk(clk491520),
	 .rst(rst_ip),
	 
	 .data_i(fifo_data),
	 .we_i(fifo_ack[0]),
	 
	 .pop_i(dac_pop_o),
	 .offset_i(4'b0),
	 .data_o(rb_data_o));
reg rb_ack_ff;

always @(posedge clk491520) begin
	if (rst_ip)
		rb_ack_ff <= 0;
	else
		rb_ack_ff <= dac_pop_o;
end

dac_drv dac_drv(
    .clk(clk491520),
    .rst(rst_ip),

    .bck_o(dac_bck_o),
    .lrck_o(dac_lrck_o),
    .data_o(dac_data_o),

    .ack_i(rb_ack_ff),
    .data_i(rb_data_o),
    .pop_o(dac_pop_o)
    );
`endif
assign dac_sck_o = clk245760;//_pad;

assign led_o = g[0].dai_locked;

assign debug_o[0] = dac_sck_o;
assign debug_o[1] = dac_lrck_o;
assign debug_o[2] = dac_data_o;
assign debug_o[3] = dac_bck_o;

endmodule
