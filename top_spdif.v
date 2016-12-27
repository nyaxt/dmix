`timescale 1ns / 1ps

module top_spdif(
    input wire clk,
    input wire rst,
    
    input wire signal_i,
    output wire ack_o,
    output wire [23:0] data_o
    );
    

spdif_dai dai(
    .clk(clk), .rst(rst),
    .signal_i(signal_i),
    .ack_o(ack_o), .data_o(data_o));

endmodule
