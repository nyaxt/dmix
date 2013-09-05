`timescale 1ns / 1ps

module spi_trx_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg sck;
parameter TCLK_SCK = 80;
reg mosi;
reg ss;

reg [7:0] data_i;
reg ack_i;

spi_trx uut(
    .clk(clk),

    .sck(sck),
    .mosi(mosi),
    .ss(ss),

    .data_i(data_i),
    .ack_i(ack_i));

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
    $dumpfile("spi_trx_t.lxt");
    $dumpvars(0, spi_trx_t);

    sck = 0;
    mosi = 0;
    ss = 1;
    ack_i = 0;

    #(TCLK*3);

    data_i = 8'hde;
    ack_i = 1;
    #(TCLK);
    ack_i = 0;
    #(TCLK);

    ss = 0;
    spi_cycle(8'hab);
    spi_cycle(8'hcd);
    spi_cycle(8'hef);

    #(TCLK*3);
    $finish(2);
end

always @(posedge clk) begin
    if(uut.ack_o)
        $display("uut.data_o: %x", uut.data_o);
end

endmodule
