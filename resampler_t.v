`timescale 1ns / 1ps
`define NUM_CH 2
`define NUM_CH_LOG2 1
`define HALFDEPTH_LOG2 4
// `define NODUMP

module resampler_t;

parameter DATALEN = 100000;
reg signed [15:0] testdata [DATALEN-1:0];
reg [16:0] testdata_iter;

reg [23:0] simple_increment_ff;

parameter TCLK = 10; // 98.304Mhz ~ 100Mhz

reg clk;
reg rst;

reg [(`NUM_CH-1):0] ack_i;
reg [(24*`NUM_CH-1):0] data_i;

reg [(`NUM_CH-1):0] pop_i;

wire [11:0] bank_addr;
wire [23:0] bank_data;
rom_firbank_441_480 bank(
    .clk(clk), .addr(bank_addr), .data(bank_data));

ringbuffered_resampler #(.NUM_CH(`NUM_CH), .NUM_CH_LOG2(`NUM_CH_LOG2)) uut(
    .clk(clk), .rst(rst),
    .bank_addr_o(bank_addr), .bank_data_i(bank_data),
    .ack_i(ack_i), .data_i(data_i),
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
    simple_increment_ff = 0;

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

always @(posedge uut.rb_pop[0]) begin
    #(TCLK);
    data_i = {24'h0, testdata[testdata_iter]};
    testdata_iter = testdata_iter+1;
    ack_i[0] = 1;
    #(TCLK);
    ack_i[0] = 0;
end

always @(posedge uut.rb_pop[1]) begin
    #(TCLK);
    data_i = {simple_increment_ff, 24'h0};
    simple_increment_ff = simple_increment_ff + 1;
    ack_i[1] = 1;
    #(TCLK);
    ack_i[1] = 0;
end

endmodule
