`timescale 1ns / 1ps

module top_spdif(
    input clk,
    input rst,
    
    input signal_i,
    output ack_o,
    output [23:0] data_o
    );
    

spdif_dai dai(
    .clk(clk), .rst(rst),
    .signal_i(signal_i),
    .ack_o(ack_o), .data_o(data_o));

endmodule
