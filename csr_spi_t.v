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

parameter NUM_CH = 8;
parameter NUM_SPDIF_IN = 3;
parameter NUM_RATE = 5;

wire [(NUM_RATE*NUM_SPDIF_IN-1):0] rate_i = {5'b00001, 5'b00100, 5'b10000};
wire [(192*NUM_SPDIF_IN-1):0] udata_i = {NUM_SPDIF_IN{192'h0102030405060708090a0b0c0d0e0f1011121314151617}};
wire [(192*NUM_SPDIF_IN-1):0] cdata_i = {NUM_SPDIF_IN{192'h0102030405060708090a0b0c0d0e0f1011121314151617}};

csr_spi uut(
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
    $display("vol_ff: %x", uut.csr.vol_ff);

    #(TCLK*3);
    ss = 0;
    spi_cycle(8'h80);
    spi_cycle(8'h03);
    spi_cycle(8'h99);
    ss = 1;

    #(TCLK*3);
    $display("after csr[12'h003] <= 8'h99");
    $display("vol_ff: %x", uut.csr.vol_ff);

    #(TCLK*3);
    ss = 0;
    spi_cycle(8'h00); // init read high  0
    spi_cycle(8'h00); //      read  low 00
    spi_cycle(8'h00); // read result 000
    spi_cycle(8'h00); // read result 001
    spi_cycle(8'h00); // read result 002
    spi_cycle(8'h00); // read result 003
    spi_cycle(8'h00); // read result 004
    spi_cycle(8'h00); // read result 005
    spi_cycle(8'h00); // read result 006
    spi_cycle(8'h00); // read result 007
    ss = 1;
    #(TCLK*3);
    $display("---");
    #(TCLK*3);
    ss = 0;
    spi_cycle(8'h08); // init read high  8
    spi_cycle(8'h01); //            low 01
    spi_cycle(8'h00); // read result 102
    spi_cycle(8'h00); // read result 103
    ss = 1;

    $finish(2);
end

always @(posedge clk) begin
    if(uut.spi_trx.ack_i)
        $display("uut.data_i: %x", uut.spi_trx.data_i);
    if(uut.spi_trx.ack_pop_o)
        $display("uut.data_o: %x", uut.spi_trx.data_o);
end

endmodule
