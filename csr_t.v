`timescale 1ns / 1ps

module csr_t;

reg clk;
parameter TCLK = 20;
always #(TCLK/2) clk = ~clk;

reg rst;

reg [11:0] addr_i;
reg ack_i;
reg [7:0] data_i;

wire [7:0] rate_i = 8'hab;
wire [383:0] udata_i;
wire [383:0] cdata_i;

csr #(.NUM_CH(2)) uut(
    .clk(clk), .rst(rst),
    .addr_i(addr_i), .ack_i(ack_i), .data_i(data_i),
    .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i));

initial begin
    $dumpfile("csr_t.lxt");
    $dumpvars(0, csr_t);

    clk = 0;
    rst = 0;

    #(TCLK*3);
    rst = 1;
    #TCLK;
    rst = 0;
    #(TCLK*3);
    uut.vol_ff = 64'h0123456789abcdef;

    addr_i = 12'h000;
    #(TCLK);
    addr_i = 12'h800;
    #(TCLK);

    $finish(2);
end

always @(posedge clk) begin
    $display("data_o: %x", uut.data_o);
end

endmodule
