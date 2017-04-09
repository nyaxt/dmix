`default_nettype none
`timescale 1ns / 1ps

//`define HWTEST

module nkmdhpa#(
    parameter NUM_CH = 2,
    parameter NUM_SPDIF_IN = 1,
    parameter NUM_RATE = 5,

    parameter VOL_WIDTH = NUM_CH*32,
    parameter NKMDDBG_WIDTH = 16*8,
    parameter RATE_WIDTH = NUM_SPDIF_IN*NUM_RATE,
    parameter UDATA_WIDTH = NUM_SPDIF_IN*192,
    parameter CDATA_WIDTH = UDATA_WIDTH,

    parameter C1_NUM_DQ_PINS = 16, // External memory data width
    parameter C1_MEM_ADDR_WIDTH = 13,  // External memory address width
    parameter C1_MEM_BANKADDR_WIDTH = 3, // External memory bank address width
    parameter C3_NUM_DQ_PINS = 16, // External memory data width
    parameter C3_MEM_ADDR_WIDTH = 13, // External memory address width
    parameter C3_MEM_BANKADDR_WIDTH = 3 // External memory bank address width
)(
    input wire clk245760_pad,
    input wire clk100m_pad,
    input wire rst,

    input wire spdif_i,

    output wire led_locked,
    output wire [3:0] led,

    output wire dac_mclk_o,
    output wire dac_lrck_o,
    output wire dac_sda_o,
    output wire dac_sck_o,

    input wire csr_sck,
    output wire csr_miso,
    input wire csr_mosi,
    input wire csr_ss,

    input wire nkmd_uart_rx,
    output wire nkmd_uart_tx,

    output wire [5:0] lcd_r,
    output wire [5:0] lcd_g,
    output wire [5:0] lcd_b,
    output wire lcd_vsync,
    output wire lcd_hsync,
    output wire lcd_nclk,
    output wire lcd_de,

    inout wire [C1_NUM_DQ_PINS-1:0] mcb1_dram_dq,
    output wire [C1_MEM_ADDR_WIDTH-1:0] mcb1_dram_a,
    output wire [C1_MEM_BANKADDR_WIDTH-1:0] mcb1_dram_ba,
    output wire mcb1_dram_ras_n,
    output wire mcb1_dram_cas_n,
    output wire mcb1_dram_we_n,
    output wire mcb1_dram_odt,
    output wire mcb1_dram_reset_n,
    output wire mcb1_dram_cke,
    output wire mcb1_dram_dm,
    inout wire mcb1_dram_udqs,
    inout wire mcb1_dram_udqs_n,
    inout wire mcb1_rzq,
    inout wire mcb1_zio,
    output wire mcb1_dram_udm,
    inout wire mcb1_dram_dqs,
    inout wire mcb1_dram_dqs_n,
    output wire mcb1_dram_ck,
    output wire mcb1_dram_ck_n,
    inout wire [C3_NUM_DQ_PINS-1:0] mcb3_dram_dq,
    output wire [C3_MEM_ADDR_WIDTH-1:0] mcb3_dram_a,
    output wire [C3_MEM_BANKADDR_WIDTH-1:0] mcb3_dram_ba,
    output wire mcb3_dram_ras_n,
    output wire mcb3_dram_cas_n,
    output wire mcb3_dram_we_n,
    output wire mcb3_dram_odt,
    output wire mcb3_dram_reset_n,
    output wire mcb3_dram_cke,
    output wire mcb3_dram_dm,
    inout wire mcb3_dram_udqs,
    inout wire mcb3_dram_udqs_n,
    inout wire mcb3_rzq,
    inout wire mcb3_zio,
    output wire mcb3_dram_udm,
    inout wire mcb3_dram_dqs,
    inout wire mcb3_dram_dqs_n,
    output wire mcb3_dram_ck,
    output wire mcb3_dram_ck_n);

wire clk245760;
wire clk491520;
wire clk983040;
dmix_dcm dcm(
    .clk245760_pad(clk245760_pad),
	 .rst_dcm(rst),
    .clk245760(clk245760),
    .clk491520(clk491520),
    .clk983040(clk983040));
`ifdef HWTEST
assign led_locked = 1'b1;

reg [22:0] counter_ff;
always @(posedge clk245760_pad)
  counter_ff <= counter_ff + 1;

assign led[2:0] = counter_ff[22:20];
assign led[3] = rst;

assign dac_mclk_o = counter_ff[16];
assign dac_lrck_o = counter_ff[18];
assign dac_sda_o = counter_ff[17];
assign dac_sck_o = counter_ff[15];
assign csr_miso = counter_ff[18];
assign nkmd_uart_tx = counter_ff[18];
`else
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

reg [5:0] rst_delay_counter;
always @(posedge clk245760) begin
    if (rst) begin
        rst_delay_counter <= 0;
    end else if(rst_delay_counter != 6'h3f) begin
        rst_delay_counter <= rst_delay_counter + 1;
    end
end

reg rst_delayed_ff;
reg rst_delayed2_ff;
always @(posedge clk245760) begin
    rst_delayed_ff <= rst_delay_counter == 6'h1f;
    rst_delayed2_ff <= rst_delay_counter == 6'h3e;
end
wire rst_dram = rst_delayed_ff;
wire rst_ip = rst_delayed2_ff;

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
// - csr <-> sd3
wire [27:0] csr_sd3_addr;
wire [31:0] csr_sd3_data;
wire csr_sd3_we;
wire csr_sd3_pop;
wire [31:0] sd3_csr_data;
wire sd3_csr_ack;
wire sd3_csr_busy;

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
    .prom_ack_o(csr_nkmd_prog_ack),

    // sd3
    .dram0_addr_o(csr_sd3_addr),
    .dram0_data_o(csr_sd3_data),
    .dram0_we_o(csr_sd3_we),
    .dram0_pop_o(csr_sd3_pop),
    .dram0_data_i(sd3_csr_data),
    .dram0_ack_i(sd3_csr_ack),
    .dram0_busy_i(sd3_csr_busy));

assign led[0] = rst_ip;
assign led[1] = csr_miso;
assign led[2] = csr_mosi;
assign led[3] = csr_ss;

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
    .dai_data_i(24'b0),
    .dai_ack_i(1'b0),
    // output wire [23:0] dai_data_o,
    .dai_pop_i(1'b0),
    // output wire dai_ack_o,

`ifdef PROMW
    .prog_addr_i(csr_nkmd_prog_addr),
    .prog_data_i(csr_nkmd_prog_data),
    .prog_ack_i(csr_nkmd_prog_ack),
`endif

    .dbgout_o(nkmd_csr_dbgout),
    .dbgin_i(csr_nkmd_dbgin));
`endif

wire [8:0] x_lcdc_fb;
wire [6:0] y_lcdc_fb;
wire pop_lcdc_fb;
wire [5:0] r_fb_lcdc;
wire [5:0] g_fb_lcdc;
wire [5:0] b_fb_lcdc;
wire ack_fb_lcdc;

lcdc lcdc(
    .clk(clk491520),
    .rst(rst_ip),

    .x_o(x_lcdc_fb),
    .y_o(y_lcdc_fb),
    .pop_o(pop_lcdc_fb),
    .r_i(r_fb_lcdc),
    .g_i(g_fb_lcdc),
    .b_i(b_fb_lcdc),
    .ack_i(ack_fb_lcdc),

    .lcd_r(lcd_r),
    .lcd_g(lcd_g),
    .lcd_b(lcd_b),
    .lcd_vsync(lcd_vsync),
    .lcd_hsync(lcd_hsync),
    .lcd_nclk(lcd_nclk),
    .lcd_de(lcd_de));

`ifdef NO_DRAM
patterngen patterngen(
    .clk(clk491520),
    .rst(rst_ip),

    .x_i(x_lcdc_fb),
    .y_i(y_lcdc_fb),
    .pop_i(pop_lcdc_fb),
    .r_o(r_fb_lcdc),
    .g_o(g_fb_lcdc),
    .b_o(b_fb_lcdc),
    .ack_o(ack_fb_lcdc));
`else
wire fb_mig_cmd_clk;
wire fb_mig_cmd_en;
wire [2:0] fb_mig_cmd_instr;
wire [5:0] fb_mig_cmd_bl;
wire [29:0] fb_mig_cmd_byte_addr;
wire mig_fb_cmd_empty;
wire mig_fb_cmd_full;
wire fb_mig_wr_clk;
wire fb_mig_wr_en;
wire [3:0] fb_mig_wr_mask;
wire [31:0] fb_mig_wr_data;
wire mig_fb_wr_full;
wire mig_fb_wr_empty;
wire [6:0] mig_fb_wr_count;
wire mig_fb_wr_underrun;
wire mig_fb_wr_error;
wire fb_mig_rd_clk;
wire fb_mig_rd_en;
wire [31:0] mig_fb_rd_data;
wire mig_fb_rd_full;
wire mig_fb_rd_empty;
wire [6:0] mig_fb_rd_count;
wire mig_fb_rd_overflow;
wire mig_fb_rd_error;

ddr3_fb ddr3_fb(
    .clk(clk491520),
    .rst(rst_ip),

    // MIG interface
    .mig_cmd_clk(fb_mig_cmd_clk),
    .mig_cmd_en(fb_mig_cmd_en),
    .mig_cmd_instr(fb_mig_cmd_instr),
    .mig_cmd_bl(fb_mig_cmd_bl),
    .mig_cmd_byte_addr(fb_mig_cmd_byte_addr),
    .mig_cmd_empty(mig_fb_cmd_empty),
    .mig_cmd_full(mig_fb_cmd_full),
    .mig_wr_clk(fb_mig_wr_clk),
    .mig_wr_en(fb_mig_wr_en),
    .mig_wr_mask(fb_mig_wr_mask),
    .mig_wr_data(fb_mig_wr_data),
    .mig_wr_full(mig_fb_wr_full),
    .mig_wr_empty(mig_fb_wr_empty),
    .mig_wr_count(mig_fb_wr_count),
    .mig_wr_underrun(mig_fb_wr_underrun),
    .mig_wr_error(mig_fb_wr_error),
    .mig_rd_clk(fb_mig_rd_clk),
    .mig_rd_en(fb_mig_rd_en),
    .mig_rd_data(mig_fb_rd_data),
    .mig_rd_full(mig_fb_rd_full),
    .mig_rd_empty(mig_fb_rd_empty),
    .mig_rd_count(mig_fb_rd_count),
    .mig_rd_overflow(mig_fb_rd_overflow),
    .mig_rd_error(mig_fb_rd_error),

    // To LCDC
    .x_i(x_lcdc_fb),
    .y_i(y_lcdc_fb),
    .pop_i(pop_lcdc_fb),
    .r_o(r_fb_lcdc),
    .g_o(g_fb_lcdc),
    .b_o(b_fb_lcdc),
    .ack_o(ack_fb_lcdc));
`endif

wire sd3_mig_cmd_clk;
wire sd3_mig_cmd_en;
wire [2:0] sd3_mig_cmd_instr;
wire [5:0] sd3_mig_cmd_bl;
wire [29:0] sd3_mig_cmd_byte_addr;
wire mig_sd3_cmd_empty;
wire mig_sd3_cmd_full;
wire sd3_mig_wr_clk;
wire sd3_mig_wr_en;
wire [3:0] sd3_mig_wr_mask;
wire [31:0] sd3_mig_wr_data;
wire mig_sd3_wr_full;
wire mig_sd3_wr_empty;
wire [6:0] mig_sd3_wr_count;
wire mig_sd3_wr_underrun;
wire mig_sd3_wr_error;
wire sd3_mig_rd_clk;
wire sd3_mig_rd_en;
wire [31:0] mig_sd3_rd_data;
wire mig_sd3_rd_full;
wire mig_sd3_rd_empty;
wire [6:0] mig_sd3_rd_count;
wire mig_sd3_rd_overflow;
wire mig_sd3_rd_error;

simple_ddr3 sd3(
    .clk(clk491520),
    .rst(rst_ip),

    .addr_i(csr_sd3_addr),
    .data_i(csr_sd3_data),
    .we_i(csr_sd3_we),
    .pop_i(csr_sd3_pop),
    .data_o(sd3_csr_data),
    .ack_o(sd3_csr_ack),
    .busy_o(sd3_csr_busy),

    // MIG interface
    .mig_cmd_clk(sd3_mig_cmd_clk),
    .mig_cmd_en(sd3_mig_cmd_en),
    .mig_cmd_instr(sd3_mig_cmd_instr),
    .mig_cmd_bl(sd3_mig_cmd_bl),
    .mig_cmd_byte_addr(sd3_mig_cmd_byte_addr),
    .mig_cmd_empty(mig_sd3_cmd_empty),
    .mig_cmd_full(mig_sd3_cmd_full),
    .mig_wr_clk(sd3_mig_wr_clk),
    .mig_wr_en(sd3_mig_wr_en),
    .mig_wr_mask(sd3_mig_wr_mask),
    .mig_wr_data(sd3_mig_wr_data),
    .mig_wr_full(mig_sd3_wr_full),
    .mig_wr_empty(mig_sd3_wr_empty),
    .mig_wr_count(mig_sd3_wr_count),
    .mig_wr_underrun(mig_sd3_wr_underrun),
    .mig_wr_error(mig_sd3_wr_error),
    .mig_rd_clk(sd3_mig_rd_clk),
    .mig_rd_en(sd3_mig_rd_en),
    .mig_rd_data(mig_sd3_rd_data),
    .mig_rd_full(mig_sd3_rd_full),
    .mig_rd_empty(mig_sd3_rd_empty),
    .mig_rd_count(mig_sd3_rd_count),
    .mig_rd_overflow(mig_sd3_rd_overflow),
    .mig_rd_error(mig_sd3_rd_error));

wire dram_sys_clk;

`ifndef NO_DRAM
dcm_dram dcm_dram(
    .clk100m(clk100m_pad),
	 .clk_dram(dram_sys_clk));

nkmd_ddr3 #(
    .C1_P0_MASK_SIZE(4),
    .C1_P0_DATA_PORT_SIZE(32),
    .C1_P1_MASK_SIZE(4),
    .C1_P1_DATA_PORT_SIZE(32),
    .DEBUG_EN(0),
    .C1_MEMCLK_PERIOD(3000),
    .C1_CALIB_SOFT_IP("TRUE"),
    .C1_SIMULATION("FALSE"),
    .C1_RST_ACT_LOW(0),
    .C1_INPUT_CLK_TYPE("SINGLE_ENDED"),
    .C1_MEM_ADDR_ORDER("ROW_BANK_COLUMN"),
    .C1_NUM_DQ_PINS(16),
    .C1_MEM_ADDR_WIDTH(13),
    .C1_MEM_BANKADDR_WIDTH(3),
    .C3_P0_MASK_SIZE(4),
    .C3_P0_DATA_PORT_SIZE(32),
    .C3_P1_MASK_SIZE(4),
    .C3_P1_DATA_PORT_SIZE(32),
    .C3_MEMCLK_PERIOD(3000),
    .C3_CALIB_SOFT_IP("TRUE"),
    .C3_SIMULATION("FALSE"),
    .C3_RST_ACT_LOW(0),
    .C3_INPUT_CLK_TYPE("SINGLE_ENDED"),
    .C3_MEM_ADDR_ORDER("ROW_BANK_COLUMN"),
    .C3_NUM_DQ_PINS(16),
    .C3_MEM_ADDR_WIDTH(13),
    .C3_MEM_BANKADDR_WIDTH(3)
)
ddr3 (
    .c1_sys_clk(dram_sys_clk),
    .c1_sys_rst_i(rst_dram),

    .mcb1_dram_dq(mcb1_dram_dq),
    .mcb1_dram_a(mcb1_dram_a),
    .mcb1_dram_ba(mcb1_dram_ba),
    .mcb1_dram_ras_n(mcb1_dram_ras_n),
    .mcb1_dram_cas_n(mcb1_dram_cas_n),
    .mcb1_dram_we_n(mcb1_dram_we_n),
    .mcb1_dram_odt(mcb1_dram_odt),
    .mcb1_dram_cke(mcb1_dram_cke),
    .mcb1_dram_ck(mcb1_dram_ck),
    .mcb1_dram_ck_n(mcb1_dram_ck_n),
    .mcb1_dram_dqs(mcb1_dram_dqs),
    .mcb1_dram_dqs_n(mcb1_dram_dqs_n),
    .mcb1_dram_udqs(mcb1_dram_udqs), // for X16 parts
    .mcb1_dram_udqs_n(mcb1_dram_udqs_n), // for X16 parts
    .mcb1_dram_udm(mcb1_dram_udm), // for X16 parts
    .mcb1_dram_dm(mcb1_dram_dm),
    .mcb1_dram_reset_n(mcb1_dram_reset_n),

    /*
    .c1_clk0(c1_clk0),
    .c1_rst0(c1_rst0),
    .c1_calib_done(c1_calib_done),
    */

    .mcb1_rzq(mcb1_rzq),
    .mcb1_zio(mcb1_zio),

    .c1_p0_cmd_clk(sd3_mig_cmd_clk),
    .c1_p0_cmd_en(sd3_mig_cmd_en),
    .c1_p0_cmd_instr(sd3_mig_cmd_instr),
    .c1_p0_cmd_bl(sd3_mig_cmd_bl),
    .c1_p0_cmd_byte_addr(sd3_mig_cmd_byte_addr),
    .c1_p0_cmd_empty(mig_sd3_cmd_empty),
    .c1_p0_cmd_full(mig_sd3_cmd_full),
    .c1_p0_wr_clk(sd3_mig_wr_clk),
    .c1_p0_wr_en(sd3_mig_wr_en),
    .c1_p0_wr_mask(sd3_mig_wr_mask),
    .c1_p0_wr_data(sd3_mig_wr_data),
    .c1_p0_wr_full(mig_sd3_wr_full),
    .c1_p0_wr_empty(mig_sd3_wr_empty),
    .c1_p0_wr_count(mig_sd3_wr_count),
    .c1_p0_wr_underrun(mig_sd3_wr_underrun),
    .c1_p0_wr_error(mig_sd3_wr_error),
    .c1_p0_rd_clk(sd3_mig_rd_clk),
    .c1_p0_rd_en(sd3_mig_rd_en),
    .c1_p0_rd_data(mig_sd3_rd_data),
    .c1_p0_rd_full(mig_sd3_rd_full),
    .c1_p0_rd_empty(mig_sd3_rd_empty),
    .c1_p0_rd_count(mig_sd3_rd_count),
    .c1_p0_rd_overflow(mig_sd3_rd_overflow),
    .c1_p0_rd_error(mig_sd3_rd_error),

    .c1_p1_cmd_clk(fb_mig_cmd_clk),
    .c1_p1_cmd_en(fb_mig_cmd_en),
    .c1_p1_cmd_instr(fb_mig_cmd_instr),
    .c1_p1_cmd_bl(fb_mig_cmd_bl),
    .c1_p1_cmd_byte_addr(fb_mig_cmd_byte_addr),
    .c1_p1_cmd_empty(mig_fb_cmd_empty),
    .c1_p1_cmd_full(mig_fb_cmd_full),
    .c1_p1_wr_clk(fb_mig_wr_clk),
    .c1_p1_wr_en(fb_mig_wr_en),
    .c1_p1_wr_mask(fb_mig_wr_mask),
    .c1_p1_wr_data(fb_mig_wr_data),
    .c1_p1_wr_full(mig_fb_wr_full),
    .c1_p1_wr_empty(mig_fb_wr_empty),
    .c1_p1_wr_count(mig_fb_wr_count),
    .c1_p1_wr_underrun(mig_fb_wr_underrun),
    .c1_p1_wr_error(mig_fb_wr_error),
    .c1_p1_rd_clk(fb_mig_rd_clk),
    .c1_p1_rd_en(fb_mig_rd_en),
    .c1_p1_rd_data(mig_fb_rd_data),
    .c1_p1_rd_full(mig_fb_rd_full),
    .c1_p1_rd_empty(mig_fb_rd_empty),
    .c1_p1_rd_count(mig_fb_rd_count),
    .c1_p1_rd_overflow(mig_fb_rd_overflow),
    .c1_p1_rd_error(mig_fb_rd_error),

    .c1_p2_cmd_clk(1'b0),
    .c1_p2_cmd_en(1'b0),
    .c1_p2_cmd_instr(3'b0),
    .c1_p2_cmd_bl(6'b0),
    .c1_p2_cmd_byte_addr(30'b0),
    // .c1_p2_cmd_empty(c1_p2_cmd_empty),
    // .c1_p2_cmd_full(c1_p2_cmd_full),
    .c1_p2_wr_clk(1'b0),
    .c1_p2_wr_en(1'b0),
    .c1_p2_wr_mask(4'b0),
    .c1_p2_wr_data(32'b0),
    // .c1_p2_wr_full(c1_p2_wr_full),
    // .c1_p2_wr_empty(c1_p2_wr_empty),
    // .c1_p2_wr_count(c1_p2_wr_count),
    // .c1_p2_wr_underrun(c1_p2_wr_underrun),
    // .c1_p2_wr_error(c1_p2_wr_error),
    .c1_p2_rd_clk(1'b0),
    .c1_p2_rd_en(1'b0),
    // .c1_p2_rd_data(c1_p2_rd_data),
    // .c1_p2_rd_full(c1_p2_rd_full),
    // .c1_p2_rd_empty(c1_p2_rd_empty),
    // .c1_p2_rd_count(c1_p2_rd_count),
    // .c1_p2_rd_overflow(c1_p2_rd_overflow),
    // .c1_p2_rd_error(c1_p2_rd_error),

    .c1_p3_cmd_clk(1'b0),
    .c1_p3_cmd_en(1'b0),
    .c1_p3_cmd_instr(3'b0),
    .c1_p3_cmd_bl(6'b0),
    .c1_p3_cmd_byte_addr(30'b0),
    // .c1_p3_cmd_empty(c1_p3_cmd_empty),
    // .c1_p3_cmd_full(c1_p3_cmd_full),
    .c1_p3_wr_clk(1'b0),
    .c1_p3_wr_en(1'b0),
    .c1_p3_wr_mask(4'b0),
    .c1_p3_wr_data(32'b0),
    // .c1_p3_wr_full(c1_p3_wr_full),
    // .c1_p3_wr_empty(c1_p3_wr_empty),
    // .c1_p3_wr_count(c1_p3_wr_count),
    // .c1_p3_wr_underrun(c1_p3_wr_underrun),
    // .c1_p3_wr_error(c1_p3_wr_error),
    .c1_p3_rd_clk(1'b0),
    .c1_p3_rd_en(1'b0),
    // .c1_p3_rd_data(c1_p3_rd_data),
    // .c1_p3_rd_full(c1_p3_rd_full),
    // .c1_p3_rd_empty(c1_p3_rd_empty),
    // .c1_p3_rd_count(c1_p3_rd_count),
    // .c1_p3_rd_overflow(c1_p3_rd_overflow),
    // .c1_p3_rd_error(c1_p3_rd_error),

    .c3_sys_clk(dram_sys_clk),
    .c3_sys_rst_i(rst_dram),

    .mcb3_dram_dq(mcb3_dram_dq),
    .mcb3_dram_a(mcb3_dram_a),
    .mcb3_dram_ba(mcb3_dram_ba),
    .mcb3_dram_ras_n(mcb3_dram_ras_n),
    .mcb3_dram_cas_n(mcb3_dram_cas_n),
    .mcb3_dram_we_n(mcb3_dram_we_n),
    .mcb3_dram_odt(mcb3_dram_odt),
    .mcb3_dram_cke(mcb3_dram_cke),
    .mcb3_dram_ck(mcb3_dram_ck),
    .mcb3_dram_ck_n(mcb3_dram_ck_n),
    .mcb3_dram_dqs(mcb3_dram_dqs),
    .mcb3_dram_dqs_n(mcb3_dram_dqs_n),
    .mcb3_dram_udqs(mcb3_dram_udqs),    // for X16 parts
    .mcb3_dram_udqs_n(mcb3_dram_udqs_n),  // for X16 parts
    .mcb3_dram_udm(mcb3_dram_udm),     // for X16 parts
    .mcb3_dram_dm(mcb3_dram_dm),
    .mcb3_dram_reset_n(mcb3_dram_reset_n),

    /*
    .c3_clk0(c3_clk0),
    .c3_rst0(c3_rst0),
    .c3_calib_done(c3_calib_done),
    */
    .mcb3_rzq(mcb3_rzq),
    .mcb3_zio(mcb3_zio),

    .c3_p0_cmd_clk(1'b0),
    .c3_p0_cmd_en(1'b0),
    .c3_p0_cmd_instr(3'b0),
    .c3_p0_cmd_bl(6'b0),
    .c3_p0_cmd_byte_addr(30'b0),
    // .c3_p0_cmd_empty(c3_p0_cmd_empty),
    // .c3_p0_cmd_full(c3_p0_cmd_full),
    .c3_p0_wr_clk(1'b0),
    .c3_p0_wr_en(1'b0),
    .c3_p0_wr_mask(4'b0),
    .c3_p0_wr_data(32'b0),
    // .c3_p0_wr_full(c3_p0_wr_full),
    // .c3_p0_wr_empty(c3_p0_wr_empty),
    // .c3_p0_wr_count(c3_p0_wr_count),
    // .c3_p0_wr_underrun(c3_p0_wr_underrun),
    // .c3_p0_wr_error(c3_p0_wr_error),
    .c3_p0_rd_clk(1'b0),
    .c3_p0_rd_en(1'b0),
    // .c3_p0_rd_data(c3_p0_rd_data),
    // .c3_p0_rd_full(c3_p0_rd_full),
    // .c3_p0_rd_empty(c3_p0_rd_empty),
    // .c3_p0_rd_count(c3_p0_rd_count),
    // .c3_p0_rd_overflow(c3_p0_rd_overflow),
    // .c3_p0_rd_error(c3_p0_rd_error),

    .c3_p1_cmd_clk(1'b0),
    .c3_p1_cmd_en(1'b0),
    .c3_p1_cmd_instr(3'b0),
    .c3_p1_cmd_bl(6'b0),
    .c3_p1_cmd_byte_addr(30'b0),
    // .c3_p1_cmd_empty(c3_p1_cmd_empty),
    // .c3_p1_cmd_full(c3_p1_cmd_full),
    .c3_p1_wr_clk(1'b0),
    .c3_p1_wr_en(1'b0),
    .c3_p1_wr_mask(4'b0),
    .c3_p1_wr_data(32'b0),
    // .c3_p1_wr_full(c3_p1_wr_full),
    // .c3_p1_wr_empty(c3_p1_wr_empty),
    // .c3_p1_wr_count(c3_p1_wr_count),
    // .c3_p1_wr_underrun(c3_p1_wr_underrun),
    // .c3_p1_wr_error(c3_p1_wr_error),
    .c3_p1_rd_clk(1'b0),
    .c3_p1_rd_en(1'b0),
    // .c3_p1_rd_data(c3_p1_rd_data),
    // .c3_p1_rd_full(c3_p1_rd_full),
    // .c3_p1_rd_empty(c3_p1_rd_empty),
    // .c3_p1_rd_count(c3_p1_rd_count),
    // .c3_p1_rd_overflow(c3_p1_rd_overflow),
    // .c3_p1_rd_error(c3_p1_rd_error),

    .c3_p2_cmd_clk(1'b0),
    .c3_p2_cmd_en(1'b0),
    .c3_p2_cmd_instr(3'b0),
    .c3_p2_cmd_bl(6'b0),
    .c3_p2_cmd_byte_addr(30'b0),
    // .c3_p2_cmd_empty(c3_p2_cmd_empty),
    // .c3_p2_cmd_full(c3_p2_cmd_full),
    .c3_p2_wr_clk(1'b0),
    .c3_p2_wr_en(1'b0),
    .c3_p2_wr_mask(4'b0),
    .c3_p2_wr_data(32'b0),
    // .c3_p2_wr_full(c3_p2_wr_full),
    // .c3_p2_wr_empty(c3_p2_wr_empty),
    // .c3_p2_wr_count(c3_p2_wr_count),
    // .c3_p2_wr_underrun(c3_p2_wr_underrun),
    // .c3_p2_wr_error(c3_p2_wr_error),
    .c3_p2_rd_clk(1'b0),
    .c3_p2_rd_en(1'b0),
    // .c3_p2_rd_data(c3_p2_rd_data),
    // .c3_p2_rd_full(c3_p2_rd_full),
    // .c3_p2_rd_empty(c3_p2_rd_empty),
    // .c3_p2_rd_count(c3_p2_rd_count),
    // .c3_p2_rd_overflow(c3_p2_rd_overflow),
    // .c3_p2_rd_error(c3_p2_rd_error),

    .c3_p3_cmd_clk(1'b0),
    .c3_p3_cmd_en(1'b0),
    .c3_p3_cmd_instr(3'b0),
    .c3_p3_cmd_bl(6'b0),
    .c3_p3_cmd_byte_addr(30'b0),
    // .c3_p3_cmd_empty(c3_p3_cmd_empty),
    // .c3_p3_cmd_full(c3_p3_cmd_full),
    .c3_p3_wr_clk(1'b0),
    .c3_p3_wr_en(1'b0),
    .c3_p3_wr_mask(4'b0),
    .c3_p3_wr_data(32'b0),
    // .c3_p3_wr_full(c3_p3_wr_full),
    // .c3_p3_wr_empty(c3_p3_wr_empty),
    // .c3_p3_wr_count(c3_p3_wr_count),
    // .c3_p3_wr_underrun(c3_p3_wr_underrun),
    // .c3_p3_wr_error(c3_p3_wr_error),
    .c3_p3_rd_clk(1'b0),
    .c3_p3_rd_en(1'b0)//,
    // .c3_p3_rd_data(c3_p3_rd_data),
    // .c3_p3_rd_full(c3_p3_rd_full),
    // .c3_p3_rd_empty(c3_p3_rd_empty),
    // .c3_p3_rd_count(c3_p3_rd_count),
    // .c3_p3_rd_overflow(c3_p3_rd_overflow),
    // .c3_p3_rd_error(c3_p3_rd_error),
    );
`endif
// NO_DRAM

endmodule
`default_nettype wire
