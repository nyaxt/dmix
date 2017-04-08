`timescale 1ns / 1ps

module ddr3_fb_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg rst;

wire [31:0] mig_rd_data = 32'h12345678;
reg [6:0] mig_rd_count;

wire [8:0] x_lcdc_fb;
wire [6:0] y_lcdc_fb;
wire in_hsync_lcdc_fb;
wire in_vsync_lcdc_fb;
wire pop_lcdc_fb;
wire [5:0] r_fb_lcdc;
wire [5:0] g_fb_lcdc;
wire [5:0] b_fb_lcdc;
wire ack_fb_lcdc;

lcdc lcdc(
    .clk(clk),
    .rst(rst),

    .x_o(x_lcdc_fb),
    .y_o(y_lcdc_fb),
    .in_hsync_o(in_hsync_lcdc_fb),
    .in_vsync_o(in_vsync_lcdc_fb),
    .pop_o(pop_lcdc_fb),
    .r_i(r_fb_lcdc),
    .g_i(g_fb_lcdc),
    .b_i(b_fb_lcdc),
    .ack_i(ack_fb_lcdc));

ddr3_fb uut(
    .clk(clk),
    .rst(rst),

    .mig_cmd_empty(1'b1),
    .mig_cmd_full(1'b0),
    .mig_wr_full(1'b1),
    .mig_wr_empty(1'b0),
    // .mig_wr_count,
    // .mig_wr_underrun,
    // .mig_wr_error,
    .mig_rd_data(mig_rd_data),
    // .mig_rd_full,
    // .mig_rd_empty,
    .mig_rd_count(mig_rd_count),
    // .mig_rd_overflow,
    // .mig_rd_error,

    .x_i(x_lcdc_fb),
    .y_i(y_lcdc_fb),
    .in_hsync_i(in_hsync_lcdc_fb),
    .in_vsync_i(in_vsync_lcdc_fb),
    .pop_i(pop_lcdc_fb),
    .r_o(r_fb_lcdc),
    .g_o(g_fb_lcdc),
    .b_o(b_fb_lcdc),
    .ack_o(ack_fb_lcdc));

reg [31:0] i;
initial begin
    $dumpfile("ddr3_fb_t.lxt");
    $dumpvars(0, ddr3_fb_t);

    rst = 1'b1;
    #(TCLK);
    rst = 1'b0;
    mig_rd_count = 'b0;
    #(TCLK*1500000);

    $finish(2);
end

always @(posedge clk) begin
    if (uut.mig_cmd_en) begin
        #(TCLK*5)
        mig_rd_count = 'd3;
        #(TCLK)
        mig_rd_count = 'd3;
        #(TCLK)
        mig_rd_count = 'd4;
        #(TCLK)
        mig_rd_count = 'd3;
        #(TCLK)
        mig_rd_count = 'd3;
        #(TCLK)
        mig_rd_count = 'd3;
        #(TCLK)
        mig_rd_count = 'd2;
        #(TCLK)
        mig_rd_count = 'd1;
        #(TCLK)
        mig_rd_count = 'd0;
    end
end

endmodule
