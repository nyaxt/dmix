`timescale 1ns / 1ps
// `define NODUMP
`define LRCONT

module resample_pipeline_t;

parameter DATALEN = 30000;
reg signed [15:0] testdata [DATALEN-1:0];
reg [16:0] testdata_iter;
integer outf;

// ins
reg clk;
reg rst;

// data input
reg [3:0] rate_i;
reg signed [23:0] data_i;
reg [1:0] ack_i;

// data output
reg [1:0] pop_i;

resample_pipeline uut(
    .clk(clk),
    .rst(rst),
    .rate_i(rate_i),
    .data_i(data_i),
    .ack_i(ack_i),
    .pop_i(pop_i));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

initial begin
`ifndef NODUMP
    $dumpfile("resample_pipeline_t.lxt");
    $dumpvars(0, resample_pipeline_t);
`endif

    $readmemh("filterpy/sample441.hex", testdata);
    testdata_iter = 0;
    outf = $fopen("filterpy/out196.hex", "w");

    clk = 1'b0;

	rate_i = 1 << uut.RATE_48;
    data_i = 24'h0;
    ack_i = 0;

    rst = 1'b0;
    #(TCLK*6);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

`ifndef NODUMP
    #10_000_000;
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
	if(testdata_iter[7:0] == 8'hff)
		$display("iter: %d", testdata_iter);

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
`ifdef LRCONT
    pop_i = 0;
    #(TCLK*127*2);
    pop_i = 2'b01;
    #(TCLK);
    pop_i = 2'b10;
    #(TCLK);
`else
    pop_i = 0;
    #(TCLK*127);
    pop_i = 2'b01;
    #(TCLK);
    pop_i = 0;
    #(TCLK*127);
    pop_i = 2'b10;
    #(TCLK);
`endif
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
