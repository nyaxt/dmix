`include "../nkmm_const.v"

module nkmm_arch(
    input clk,
    input rst,
    
    input uart_rx,
    output uart_tx,

    output [7:0] debug_led,
    input [7:0] switch);

wire [`ACCUM_WIDTH-1:0] cpu_data_i;
wire [`ACCUM_WIDTH-1:0] cpu_data_o;
wire [`ADDR_WIDTH-1:0] cpu_addr_o;
wire cpu_we_o;

wire [`INSN_WIDTH-1:0] cpu_prog_data_i;
wire [`ADDR_WIDTH-1:0] cpu_prog_addr_o;
nkmm_cpu cpu(
    .clk(clk), .rst(rst),

    .data_i(cpu_data_i),
    .data_o(cpu_data_o),
    .addr_o(cpu_addr_o),
    .we_o(cpu_we_o),

    .prog_data_i(cpu_prog_data_i),
    .prog_addr_o(cpu_prog_addr_o));

wire [`ACCUM_WIDTH-1:0] ram_data_o;
nkmm_ram ram(
    .clk(clk),

    .data_i(cpu_data_o),
    .data_o(ram_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

wire [`ACCUM_WIDTH-1:0] uart_data_o;
nkmm_uart uart(
    .clk(clk), .rst(rst),
    
    .rx(uart_rx), .tx(uart_tx),

    .data_i(cpu_data_o),
    .data_o(uart_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

wire [`ACCUM_WIDTH-1:0] debug_data_o;
nkmm_debug debug(
    .clk(clk), .rst(rst),

    .debug_led(debug_led), .switch(switch),

    .data_i(cpu_data_o),
    .data_o(debug_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

assign cpu_data_i = ram_data_o | uart_data_o | debug_data_o;

nkmm_progrom rom(
    .clk(clk),

    .prog_addr_i(cpu_prog_addr_o),
    .prog_data_o(cpu_prog_data_i));

endmodule
