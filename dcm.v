`timescale 1ns / 1ps
// `define DEBUG_SLOW
// `define USE_IP

module dmix_dcm(
    input clk245760_pad,
    output clk245760,
    output clk491520,
    output clk983040);

`ifdef DEBUG_SLOW
reg [1:0] mul_counter_ff;
always @(posedge clk245760_pad)
    mul_counter_ff <= mul_counter_ff + 2'b1;

assign clk245760 = mul_counter_ff[1];
assign clk491520 = mul_counter_ff[0];
assign clk983040 = clk245760_pad;

`else
`ifdef USE_IP
assign clk245760 = clk245760_pad;

wire clk245760_unbuf; // =  48.0kHz * 64 bits * 32 clk/bit = 98.3040Mhz
wire clk491520_unbuf; // =  96.0kHz * 64 bits * 16 clk/bit = 98.3040Mhz
wire clk983040_unbuf; // = 192.0kHz * 64 bits *  8 clk/bit = 98.3040Mhz
DCM #(
    .CLK_FEEDBACK("1X"),
    .CLKFX_DIVIDE(1),
    .CLKFX_MULTIPLY(4),
    .CLKIN_PERIOD(40.690),
    .CLKOUT_PHASE_SHIFT("NONE"),
    .DFS_FREQUENCY_MODE("LOW"),
    .FACTORY_JF(16'hC080),
    .PHASE_SHIFT(0),
    .STARTUP_WAIT("FALSE")
) dcm983040(
    .CLKFB(clk245760_unbuf),
    .CLKIN(clk245760_pad),
    .DSSEN(0), 
    .PSCLK(0), // phase shift
    .PSEN(0),
    .PSINCDEC(0),
    .RST(rst_dcm),
    .CLK0(clk245760_unbuf),
    .CLK2X(clk491520_unbuf),
    .CLKFX(clk983040_unbuf));
BUFG buf491520(.I(clk491520_unbuf), .O(clk491520));
BUFG buf983040(.I(clk983040_unbuf), .O(clk983040));
`else

reg clk245760_ff;
reg clk491520_ff;
reg clk983040_ff;
assign clk245760 = clk245760_ff;
assign clk491520 = clk491520_ff;
assign clk983040 = clk983040_ff;

initial begin
    clk245760_ff = 0;
    clk491520_ff = 0;
    clk983040_ff = 0;
    #(250);
end

always begin
    #(1);
    clk983040_ff = ~clk983040_ff;
    #(4);
end

always begin
    #(10);
    clk491520_ff = ~clk491520_ff;
    #(0);
end

always begin
    #(5);
    clk245760_ff = ~clk245760_ff;
    #(15);
end

`endif
`endif

endmodule
