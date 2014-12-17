`timescale 1ns / 1ps

module mpemu_t;

`define DATALEN 32
reg [23:0] testdata_a [`DATALEN-1:0];
reg [23:0] testdata_b [`DATALEN-1:0];
reg [23:0] testdata_p [`DATALEN-1:0];

// ins
reg clk;

reg [23:0] mpcand_i;
reg [23:0] mplier_i;

// outs
wire [23:0] mprod_o;

mpemu uut(
    .clk(clk),
    .mpcand_i(mpcand_i), .mplier_i(mplier_i),
    .mprod_o(mprod_o));

parameter TCLK = 41.0; // ~40.69ns (24.576Mhz)

integer i;
initial begin
	$dumpfile("mpemu_t.lxt");
	$dumpvars(0, mpemu_t);

    $readmemh("testdata/gen/mp_a.hex", testdata_a);
    $readmemh("testdata/gen/mp_b.hex", testdata_b);
    $readmemh("testdata/gen/mp_p.hex", testdata_p);
	
	clk = 1'b0;
    #TCLK;

    mpcand_i = 24'h100000;
    mplier_i = 24'h123456;
    #TCLK;

    mpcand_i = 24'h123456;
    mplier_i = 24'h100000;
    #TCLK;

    mpcand_i = 24'hffffff; // -1
    mplier_i = 24'h7fffff; // 0x7fffff
    #TCLK;

    for (i = 0; i < `DATALEN; i = i + 1) begin
        mpcand_i = testdata_a[i];
        mplier_i = testdata_b[i];
        #TCLK;
    end
    #(TCLK*8);
    
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

integer i2;
always begin
    #(TCLK*6);
    $display("%h should be %h", mprod_o, 24'h02468a);
    #TCLK;
    $display("%h should be %h", mprod_o, 24'h02468a);
    #TCLK;
    $display("%h should be %h", mprod_o, 24'hffffff);
    #TCLK;
    for (i2 = 0; i2 < `DATALEN; i2 = i2 + 1) begin
        $display("%h should be %h", mprod_o, testdata_p[i2]);
        #TCLK;
    end
end

endmodule
