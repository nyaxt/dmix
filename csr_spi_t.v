`timescale 1ns / 1ps

module csr_spi_t;

// ins
reg clk;
reg rst;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg sck;
parameter TCLK_SCK = 80;
reg mosi;
reg ss;

wire [7:0] rate_i = 8'hab;
wire [383:0] udata_i;
wire [383:0] cdata_i;

csr_spi #(.NUM_CH(2)) uut(
    .clk(clk), .rst(rst),

    .sck(sck),
    .mosi(mosi),
    .ss(ss),

    .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i));

task spi_cycle;
    input [7:0] data;
    begin
        mosi = data[7];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[6];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[5];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[4];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[3];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[2];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[1];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[0];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
    end
endtask

initial begin
    $dumpfile("csr_spi_t.lxt");
    $dumpvars(0, csr_spi_t);

    sck = 0;
    mosi = 0;
    ss = 1;

    rst = 0;
    #(TCLK);
    rst = 1;
    #(TCLK);
    rst = 0;
    uut.csr.vol_ff = 64'h0123456789abcdef;

    #(TCLK*3);
    ss = 0;
    spi_cycle(8'h80);
    spi_cycle(8'h03);
    spi_cycle(8'h99);
    ss = 1;

    #(TCLK*3);
    ss = 0;
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    spi_cycle(8'h00);
    #(TCLK*3);
    $finish(2);
end

endmodule
