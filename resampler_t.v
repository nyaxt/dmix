`timescale 1ns / 1ps
`define NUM_CH 2
`define NUM_CH_LOG2 1
`define HALFDEPTH_LOG2 4
// `define NODUMP

module resampler_t;

parameter DATALEN = 100000;
reg signed [15:0] testdata [DATALEN-1:0];
reg [16:0] testdata_iter;

parameter TCLK = 10; // 98.304Mhz ~ 100Mhz

reg clk;
reg rst;

reg [(24*`NUM_CH-1):0] data_i;
reg [(`NUM_CH-1):0] ack_i;

reg [(`NUM_CH-1):0] pop_i;

wire [(`NUM_CH-1):0] rb_pop;
wire [(`HALFDEPTH_LOG2+1-1):0] rb_offset;
wire [23:0] rb_data;
ringbuf #(
    .LEN(64), // should work w/ 32, but buffer a little to address input jitter
    .LEN_LOG2(6)
) rb(
    .clk(clk), .rst(rst),
    .data_i(data_i[23:0]), .we_i(ack_i[0]),
    .pop_i(rb_pop[0]), .offset_i({1'b0, rb_offset[`HALFDEPTH_LOG2:0]}), .data_o(rb_data[23:0]));

wire [11:0] bank_addr;
wire [23:0] bank_data;
rom_firbank_441_480 bank(
    .clk(clk), .addr(bank_addr), .data(bank_data));

resampler_core #(.NUM_CH(`NUM_CH), .NUM_CH_LOG2(`NUM_CH_LOG2)) uut(
    .clk(clk), .rst(rst),
    .bank_addr_o(bank_addr), .bank_data_i(bank_data),
    .pop_o(rb_pop), .offset_o(rb_offset), .data_i(rb_data),
    .pop_i(pop_i)
    );

integer i;
initial begin
`ifndef NODUMP
    $dumpfile("resampler_t.lxt");
    $dumpvars(0, resampler_t);
`endif
    $readmemh("testdata/damashie.hex", testdata);
    testdata_iter = 0;

    clk = 1'b0;

    data_i = 24'h0;

    rst = 1'b0;
    #(TCLK);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

    for (i = 0; i < 65; i = i + 1) begin
        #(TCLK);
        data_i = {testdata[testdata_iter]};
        testdata_iter = testdata_iter+1;
        ack_i[0] = 1;
    end
    ack_i[0] = 0;

    rst = 1'b0;
    #(TCLK);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

`ifndef NODUMP
    #10_000;
    $finish(2);
`endif
end

always #(TCLK/2) clk = ~clk;

always begin
    pop_i = 2'b11;
    #(TCLK);
    pop_i = 2'b00;
    #(TCLK*63);
end

always @(posedge uut.pop_o[0]) begin
    #(TCLK);
    data_i = {24'h0, testdata[testdata_iter]};
    testdata_iter = testdata_iter+1;
    ack_i[0] = 1;
    #(TCLK);
    ack_i[0] = 0;
end

reg [23:0] simple_increment_ff;

always @(posedge uut.pop_o[1]) begin
    #(TCLK);
    data_i = {simple_increment_ff, 24'h0};
    simple_increment_ff = simple_increment_ff + 1;
    ack_i[1] = 1;
    #(TCLK);
    ack_i[1] = 0;
end

endmodule
