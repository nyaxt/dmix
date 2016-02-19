`timescale 1ns / 1ps

module uart_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

parameter TCLK_BAUD = 104167;
// reg baudclk;
// always #(TCLK/2) baudclk = ~baudclk;

reg rx;

reg [7:0] data_i;
reg ack_i;

uart uut(
    .clk(clk),

    .rx(rx),

    .data_i(data_i),
    .ack_i(ack_i));

task uart_cycle;
    input [7:0] data;
    begin
        rx = 0; // start bit
        #(TCLK_BAUD);
        rx = data[0];
        #(TCLK_BAUD);
        rx = data[1];
        #(TCLK_BAUD);
        rx = data[2];
        #(TCLK_BAUD);
        rx = data[3];
        #(TCLK_BAUD);
        rx = data[4];
        #(TCLK_BAUD);
        rx = data[5];
        #(TCLK_BAUD);
        rx = data[6];
        #(TCLK_BAUD);
        rx = data[7];
        #(TCLK_BAUD);
        rx = 1; // stop bit
        #(TCLK_BAUD);
    end
endtask

initial begin
    $dumpfile("uart_t.lxt");
    $dumpvars(0, uart_t);

    rx = 1;
    #(TCLK_BAUD*10);

    uart_cycle(8'h12);
    uart_cycle(8'h34);
    uart_cycle(8'h56);
    #(5000);

    uart_cycle(8'hab);
    #(10000);
    uart_cycle(8'hcd);
    #(200000);
    uart_cycle(8'hef);

    #(TCLK*10);
    $finish(2);
end

always @(posedge clk) begin
    if(uut.ack_pop_o)
        $display("uut.data_o: %x", uut.data_o);
end

endmodule
