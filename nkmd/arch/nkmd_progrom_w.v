`default_nettype none
`timescale 1ns / 1ps

module nkmd_progrom_w #(
    parameter WIDTH = 1024
)(
    input wire clk,

    output wire [31:0] data_o,
    input wire [31:0] addr_i,

    input wire [31:0] prog_addr_i,
    input wire [31:0] prog_data_i,
    input wire prog_ack_i);

reg [31:0] pram [(WIDTH-1):0];

reg [31:0] data_ff;
always @(posedge clk)
    data_ff <= pram[addr_i];
assign data_o = data_ff;

always @(posedge clk) begin
    if (prog_ack_i) begin
        pram[prog_addr_i] <= prog_data_i;
    end
end

endmodule
`default_nettype wire
