`default_nettype none
`timescale 1ns / 1ps

module lcdc_t;

// ins
reg clk;
reg rst;

lcdc lcdc(
    .clk(clk),
    .rst(rst),

    .r_i(6'd00),
    .g_i(6'd00),
    .b_i(6'd00),
    .ack_i(1'b0));

parameter TCLK = 20.0; // 50MHz

initial begin
    $dumpfile("lcdc_t.lxt");
    $dumpvars(0, lcdc_t);

    clk = 1'b0;

    rst = 1'b0;
    #TCLK;
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

    #(TCLK*2500000);
    $finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
`default_nettype wire
