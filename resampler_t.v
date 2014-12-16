`timescale 1ns / 1ps
`define NUM_CH 2
// `define NODUMP

module resampler_t;

parameter TCLK = 10; // 98.304Mhz ~ 100Mhz

reg clk;
reg rst;

wire [11:0] bank_addr;
wire [23:0] bank_data;
rom_firbank_441_480 bank(
    .clk(clk), .addr(bank_addr), .data(bank_data));

reg [(24*`NUM_CH-1):0] data_i;
reg [(`NUM_CH-1):0] pop_i;
resampler_core #(.NUM_CH(`NUM_CH)) uut(
    .clk(clk), .rst(rst),
    .bank_addr_o(bank_addr), .bank_data_i(bank_data),
    .data_i(data_i), .pop_i(pop_i));

initial begin
`ifndef NODUMP
    $dumpfile("resampler_t.lxt");
    $dumpvars(0, resampler_t);
`endif

    clk = 1'b0;

    data_i = 24'h0;

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
    pop_i = 2'b01;
    #(TCLK);
    pop_i = 2'b00;
    #(TCLK*63);
end

endmodule
