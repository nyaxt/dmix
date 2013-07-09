`timescale 1ns / 1ps
`define NODUMP
// `define TEST441

module resampler_t;

parameter DATALEN = 30000;
reg signed [15:0] testdata [DATALEN-1:0];
reg [16:0] testdata_iter;
integer outf;

// ins
reg clk;
reg rst;

// data input
reg signed [23:0] data_i;
reg [1:0] ack_i;

// data output
reg [1:0] pop_i;

// multiplier
reg mpready;
wire [23:0] mpcand;
wire [15:0] mplier;
wire [23:0] mprod;

mpemu mpemu(
    .clk(clk),
    .mpcand_i(mpcand), .mplier_i(mplier), .mprod_o(mprod));

`ifdef TEST441
resample441_48 uut(
    .clk(clk), .rst(rst),
    .mpready_i(mpready), .mpcand_o(mpcand), .mplier_o(mplier), .mprod_i(mprod),
    .data_i(data_i), .ack_i(ack_i),
    .pop_i(pop_i));
`else
upsample2x uut(
    .clk(clk), .rst(rst),
    .mpready_i(mpready), .mpcand_o(mpcand), .mplier_o(mplier), .mprod_i(mprod),
    .data_i(data_i), .ack_i(ack_i),
    .pop_i(pop_i));
`endif

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
`ifndef NODUMP
    $dumpfile("resampler_t.lxt");
    $dumpvars(0, resampler_t);
`endif

    $readmemh("filterpy/sample441.hex", testdata);
    testdata_iter = 0;
`ifdef TEST441
    outf = $fopen("filterpy/out48.hex", "w");
`else
    outf = $fopen("filterpy/out96.hex", "w");
`endif
    
    clk = 1'b0;

    data_i = 24'h0;
    ack_i = 0;
    mpready = 1;

    rst = 1'b0;
    #(TCLK*6);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

`ifndef NODUMP
    #1_000_000;
    $finish(2);
`endif
end

always #(TCLK/2) clk = ~clk;

reg TESTCH = 1'b1;

always @(posedge uut.pop_o[TESTCH]) begin
    #(TCLK);
    data_i = {testdata[testdata_iter], 8'h00};
`ifndef NODUMP
    $display("in data: %d", data_i >>> 16);
`endif
    testdata_iter = testdata_iter+1;
    ack_i[TESTCH] = 1;
    #(TCLK);
    ack_i[TESTCH] = 0;
    if(testdata_iter == DATALEN-1)
        $finish(2);
end

always @(posedge uut.pop_o[1-TESTCH]) begin
    #(TCLK);
    data_i = -{testdata_iter, 16'h00};
    ack_i[1-TESTCH] = 1;
    #(TCLK);
    ack_i[1-TESTCH] = 0;
end

always begin
    pop_i = 0;
    #(TCLK*63);
    pop_i = 2'b01;
    #(TCLK);
    pop_i = 0;
    #(TCLK*63);
    pop_i = 2'b10;
    #(TCLK);
end

wire signed [23:0] data_s = uut.data_o;
always @(posedge uut.ack_o[TESTCH]) begin
    #(TCLK/2);
`ifndef NODUMP
    $display("out data: %d", data_s >>> 16);
`endif
    $fwrite(outf, "%h\n", uut.data_o);
end

endmodule
