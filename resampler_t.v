`timescale 1ns / 1ps
`define NUM_CH 2
`define NUM_CH_LOG2 1

`define TEST_32_48
// `define TEST_96192
`define PRELOAD
`define NODUMP

module resampler_t;

parameter DATALEN = 100000;
reg [15:0] testdata [DATALEN-1:0];
reg [16:0] testdata_iter;

wire [15:0] testdata_curr = testdata[testdata_iter];
wire [23:0] testdata_curr_exp = {testdata_curr, 8'b0}; //{{8{testdata_curr[15]}}, testdata_curr};
reg [23:0] simple_increment_ff;

parameter TCLK = 10; // 98.304Mhz ~ 100Mhz

reg clk;
reg rst;
wire [(`NUM_CH-1):0] rst_ch = 2'b00;

reg [(`NUM_CH-1):0] ack_i;
reg [(24*`NUM_CH-1):0] data_i;
reg [(24*`NUM_CH-1):0] data_i2;

reg [(`NUM_CH-1):0] pop_i;

wire [23:0] bank_data;
`ifdef TEST_32_48
wire [5:0] bank_addr;
rom_firbank_32_48 bank_32_48(.clk(clk), .addr(bank_addr), .data(bank_data));
ringbuffered_resampler #(
    .NUM_CH(`NUM_CH), .NUM_CH_LOG2(`NUM_CH_LOG2),
    .HALFDEPTH(16), .HALFDEPTH_LOG2(4),
    .NUM_FIR(3), .NUM_FIR_LOG2(2), .DECIM(2),
    .TIMESLICE(64), .TIMESLICE_LOG2(6))
`elsif TEST_96192
wire [3:0] bank_addr;
rom_firbank_96_192 bank(.clk(clk), .addr(bank_addr), .data(bank_data));
ringbuffered_resampler #(
    .NUM_CH(`NUM_CH), .NUM_CH_LOG2(`NUM_CH_LOG2),
    .HALFDEPTH(8), .HALFDEPTH_LOG2(3),
    .NUM_FIR(2), .NUM_FIR_LOG2(1), .DECIM(1),
    .TIMESLICE(32), .TIMESLICE_LOG2(5))
`else
wire [11:0] bank_addr;
rom_firbank_441_480 bank(.clk(clk), .addr(bank_addr), .data(bank_data));
ringbuffered_resampler #(.NUM_CH(`NUM_CH), .NUM_CH_LOG2(`NUM_CH_LOG2))
`endif
  uut(
    .clk(clk), .rst(rst), .rst_ch(rst_ch),
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

`ifdef PRELOAD
    for (i = 0; i < 64; i = i + 1) begin
        #(TCLK);
        data_i = {simple_increment_ff, testdata_curr_exp};
        testdata_iter = testdata_iter + 1;
        simple_increment_ff = simple_increment_ff + 1;
        ack_i = 2'b11;
    end
    ack_i = 2'b00;
`endif

`ifndef NODUMP
    #3_000;
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
    data_i = {24'h0, testdata_curr_exp};
    testdata_iter = testdata_iter+1;
    if (testdata_iter == 256)
        $finish(2);
    ack_i[0] = 1;
    #(TCLK);
    ack_i[0] = 0;
end

always @(posedge uut.pop_o[1]) begin
    #(TCLK);
    data_i = {simple_increment_ff, 24'h0};
    simple_increment_ff = simple_increment_ff + 1;
    ack_i[1] = 1;
    #(TCLK);
    ack_i[1] = 0;
end

always @(posedge uut.ack_o[1]) begin
    $display("%d", $signed(uut.data_o));
end

endmodule
