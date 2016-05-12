module nkmd_cpu(
    input clk,
    input rst,

    // BUS R: RAM + MMAPIO
    input [31:0] r_data_i,
    output [31:0] r_data_o,
    output [31:0] r_addr_o,
    output r_we_o,

    // BUS P: PROGRAM
    input [31:0] p_data_i,
    output [31:0] p_addr_o,

    // BUS C: RAM2
    input [31:0] c_data_i,
    output [31:0] c_data_o,
    output [31:0] c_addr_o,
    output c_we_o);


endmodule
