`timescale 1ns / 1ps

module spdif_tx_t;

reg [23:0] simple_increment_ff;
reg [23:0] simple_increment_ff2;

parameter TCLK = 40;
parameter TCLK_DAI = 10;

reg clk;
reg clk_dai;
reg rst;

reg [1:0] ack_i;
reg [47:0] data_i;

spdif_tx uut(
    .clk(clk), .rst(rst),
    .ack_i(ack_i), .data_i(data_i),
    .udata_i(192'b0), .cdata_i(192'b0));
spdif_dai dai(
    .clk(clk), .rst(rst),
    .clk_per_halfbit(4),
    .signal_i(uut.spdif_o)
);

integer i;
initial begin
`ifndef NODUMP
    $dumpfile("spdif_tx_t.lxt");
    $dumpvars(0, spdif_tx_t);
`endif
    simple_increment_ff = 0;
    simple_increment_ff2 = 24'hffffff;

    uut.data_latch = 48'b0;
    uut.subframe_shiftreg = 28'b0;

    clk = 1'b0;
    clk_dai = 1'b0;
    ack_i = 2'b00;

    rst = 1'b0;
    #TCLK;
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

`ifndef NODUMP
    #500_000;
    $finish(2);
`endif
end

always #(TCLK/2) clk = ~clk;
always #(TCLK_DAI/2) clk_dai = ~clk_dai;

always @(posedge uut.pop_o[0]) begin
    #(TCLK);
    data_i[47:24] = simple_increment_ff;
    simple_increment_ff = simple_increment_ff + 1;
    if (simple_increment_ff == 256)
        $finish(2);
    ack_i[1] = 1;
    #(TCLK);
    ack_i[1] = 0;
end

always @(posedge uut.pop_o[1]) begin
    #(TCLK);
    data_i[23:0] = 24'habcdef;//simple_increment_ff2;
    simple_increment_ff2 = simple_increment_ff2 - 1;
    ack_i[0] = 1;
    #(TCLK);
    ack_i[0] = 0;
end

always @(posedge clk_dai) begin
    if(dai.ack_o) begin
        $display("data_o: %x", dai.data_o);
    end
end


endmodule
