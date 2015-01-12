`timescale 1ns / 1ps

module mpemu_scale_t;

`define DATALEN 32
reg [31:0] testdata_a [`DATALEN-1:0];
reg [31:0] testdata_b [`DATALEN-1:0];
reg [31:0] testdata_p [`DATALEN-1:0];

// ins
reg clk;

reg [23:0] mpcand_i;
reg [31:0] scale_i;

// outs
wire [31:0] mprod_o;

mpemu_scale uut(
    .clk(clk),
    .mpcand_i(mpcand_i), .scale_i(scale_i),
    .mprod_o(mprod_o));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

integer i;
initial begin
	$dumpfile("mpemu_scale_t.lxt");
	$dumpvars(0, mpemu_scale_t);

    $readmemh("testdata/gen/mp_scale_a.hex", testdata_a);
    $readmemh("testdata/gen/mp_scale_b.hex", testdata_b);
    $readmemh("testdata/gen/mp_scale_p.hex", testdata_p);
	
	clk = 1'b0;
    #TCLK;

    mpcand_i = 24'h123456;
    scale_i = 32'h01_000000;
    #TCLK;

    mpcand_i = 24'h123456;
    scale_i = 32'h02_000000;
    #TCLK;

    mpcand_i = 24'hffffff; // -1
    scale_i = 32'h01_000000;
    #TCLK;

    for (i = 0; i < `DATALEN; i = i + 1) begin
        mpcand_i = testdata_a[i];
        scale_i = testdata_b[i];
        #TCLK;
    end
    #(TCLK*8);
    
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

integer i2;
always begin
    #(TCLK*7);
    $display("%h should be %h", mprod_o, 32'h00123456);
    #TCLK;
    $display("%h should be %h", mprod_o, 32'h002468ac);
    #TCLK;
    $display("%h should be %h", mprod_o, 32'hffffffff);
    #TCLK;
    for (i2 = 0; i2 < `DATALEN; i2 = i2 + 1) begin
        $display("%h should be %h", mprod_o, testdata_p[i2]);
        #TCLK;
    end
end

endmodule
