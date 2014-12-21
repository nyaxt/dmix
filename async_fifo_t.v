`timescale 1ns / 1ps
`define SIM_FULL

module async_fifo_t;

// ins
reg wclk;
reg wrst;
reg [7:0] data_i;
reg ack_i;

reg rclk;
reg rrst;
reg pop_i;

async_fifo uut(
    .wclk(wclk), .wrst(wrst), .data_i(data_i), .ack_i(ack_i),
    .rclk(rclk), .rrst(rrst), .pop_i(pop_i));

parameter T_WCLK = 10;
parameter T_RCLK = 24;
always #(T_WCLK/2) wclk = ~wclk;
always #(T_RCLK/2) rclk = ~rclk;

initial begin
	$dumpfile("async_fifo_t.lxt");
	$dumpvars(0, async_fifo_t);

    #(3000);
	$finish(2);
end

initial begin
    wclk = 0;
    wrst = 0;
    ack_i = 0; 
    data_i = 8'h20;

    #(T_WCLK);
    wrst = 1;
    #(T_WCLK);
    wrst = 0;
end

always begin
    #(T_WCLK*15);
    data_i = data_i + 1;
    ack_i = 1; 
    #(T_WCLK);
    ack_i = 0;
end

initial begin
    rclk = 0;
    rrst = 0;
    pop_i = 0;

    #(T_RCLK);
    rrst = 1;
    #(T_RCLK);
    rrst = 0;
end

`ifndef SIM_FULL
always @(posedge rclk) begin
    pop_i <= 0;

    if (!uut.empty_o) begin
        $display("read %h", uut.data_o);
        pop_i <= 1;
    end
end
`endif

endmodule
