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
    input wire [7:0] data;
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

    data_i = 8'hZZ;
    ack_i = 0;
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
    if (uut.ack_o)
        $display("uut.data_o: %x", uut.data_o);
end

reg [7:0] send_data [0:31];
initial begin
    send_data[9'd00] = 8'h12;
    send_data[9'd01] = 8'h34;
    send_data[9'd02] = 8'h56;
    send_data[9'd03] = 8'h78;
    send_data[9'd04] = 8'hab;
    send_data[9'd05] = 8'hcd;
    send_data[9'd06] = 8'hef;
    send_data[9'd07] = 8'hde;
    send_data[9'd08] = 8'had;
    send_data[9'd09] = 8'hbe;
    send_data[9'd10] = 8'hef;
end

reg [7:0] iter;
initial iter = 0;
always @(posedge clk) begin
    if (uut.pop_o) begin
        #(TCLK);
        data_i = send_data[iter];
        iter = iter + 1;
        ack_i = 1;
        #(TCLK);
        data_i = 8'hZZ;
        iter = iter + 1;
        ack_i = 0;
    end
end

endmodule
