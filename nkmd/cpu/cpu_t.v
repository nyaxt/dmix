`default_nettype none
`timescale 1ns / 1ps

module nkmd_t;

// ins
reg clk;
reg rst;

wire [31:0] cpu_r_data_i;
assign cpu_r_data_i = 32'hdeadbeef;
wire [31:0] cpu_c_data_i;
assign cpu_c_data_i = 32'hbaadf00d;

wire [31:0] cpu_p_data_i;
wire [31:0] cpu_p_addr_o;
nkmd_cpu cpu(
    .clk(clk),
    .rst(rst),

    .r_data_i(cpu_r_data_i),
    .c_data_i(cpu_c_data_i),

    .p_data_i(cpu_p_data_i),
    .p_addr_o(cpu_p_addr_o));

parameter TCLK = 10.0; // 100MHz

initial begin
`ifndef NODUMP
    $dumpfile("nkmd_t.lxt");
    $dumpvars(0, nkmd_t);
`endif

    clk = 1'b0;

    rst = 1'b0;
    #(TCLK*6);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

`ifndef NODUMP
    #(TCLK*10);
    $finish(2);
`endif
end

always #(TCLK/2) clk = ~clk;

nkmd_progrom nkmd_progrom(
    .clk(clk),

    .addr_i(cpu_p_addr_o),
    .data_o(cpu_p_data_i));

endmodule
