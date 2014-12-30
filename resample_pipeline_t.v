`timescale 1ns / 1ps
`define NUM_CH 2
`define NUM_CH_LOG2 1

`define PRELOAD
`define NODUMP

module resample_pipeline_t;

parameter DATALEN = 100000;
reg [15:0] testdata [DATALEN-1:0];
reg [16:0] testdata_iter;

wire [15:0] testdata_curr = testdata[testdata_iter];
wire [23:0] testdata_curr_exp = {testdata_curr, 8'b0}; //{{8{testdata_curr[15]}}, testdata_curr};
reg [23:0] simple_increment_ff;

parameter TCLK = 10; // 98.304Mhz ~ 100Mhz

reg clk;
reg rst;

reg [(`NUM_CH-1):0] ack_i;
reg [(24*`NUM_CH-1):0] data_i;

reg [(`NUM_CH-1):0] pop_i;

wire [23:0] bank_data;
resample_pipeline #(.NUM_CH(2), .NUM_CH_LOG2(1)) uut(
    .clk(clk), .rst(rst),
    .rate_i(10'b0100001000),
    .ack_i(ack_i), .data_i(data_i),
    .pop_i(pop_i)
    );

integer i;
initial begin
`ifndef NODUMP
    $dumpfile("resample_pipeline_t.lxt");
    $dumpvars(0, resample_pipeline_t);
`endif
    $readmemh("testdata/damashie.hex", testdata);
    testdata_iter = 0;
    simple_increment_ff = 0;

    clk = 1'b0;

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
    data_i[23:0] = testdata_curr_exp;
    testdata_iter = testdata_iter+1;
    if (testdata_iter == 256)
        $finish(2);
    ack_i[0] = 1;
    #(TCLK);
    ack_i[0] = 0;
end

always @(posedge uut.pop_o[1]) begin
    #(TCLK);
    data_i[46:24] = simple_increment_ff;
    simple_increment_ff = simple_increment_ff + 1;
    ack_i[1] = 1;
    #(TCLK);
    ack_i[1] = 0;
end

always @(posedge uut.ack_o[1]) begin
    $display("%d", $signed(uut.data_o[46:24]));
end

endmodule
