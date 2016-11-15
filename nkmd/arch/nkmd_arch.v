module nkmd_arch(
    input clk,
    input rst,
    
    input uart_rx,
    output uart_tx,

    input [23:0] dai_data_i,
    input dai_ack_i,

    output [23:0] dai_data_o,
    input dai_pop_i,
    output dai_ack_o,

    output [7:0] debug_led,
    input [7:0] switch);

wire [31:0] cpu_data_i;
wire [31:0] cpu_data_o;
wire [31:0] cpu_addr_o;
wire cpu_we_o;

wire [31:0] cpu_c_data_i;
assign cpu_c_data_i = 32'hbaadf00d;

wire [31:0] cpu_prog_data_i;
wire [31:0] cpu_prog_addr_o;
nkmd_cpu cpu(
    .clk(clk), .rst(rst),

    .r_data_i(cpu_data_i),
    .r_data_o(cpu_data_o),
    .r_addr_o(cpu_addr_o),
    .r_we_o(cpu_we_o),

    .c_data_i(cpu_c_data_i),

    .p_data_i(cpu_prog_data_i),
    .p_addr_o(cpu_prog_addr_o));

wire [31:0] ram_data_o;
nkmd_ram ram(
    .clk(clk),
    .data_i(cpu_data_o),
    .data_o(ram_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

wire [31:0] uart_data_o;
nkmd_uart uart(
    .clk(clk), .rst(rst),
    
    .rx(uart_rx), .tx(uart_tx),

    .data_i(cpu_data_o),
    .data_o(uart_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

wire [31:0] dai_rx_data_o;
nkmd_dai_rx dai_rx(
    .clk(clk), .rst(rst),

    .rx_data_i(dai_data_i),
    .rx_ack_i(dai_ack_i),

    .data_i(cpu_data_o),
    .data_o(dai_rx_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

wire [31:0] dai_tx_data_o;
nkmd_dai_tx dai_tx(
    .clk(clk), .rst(rst),

    .tx_data_o(dai_data_o),
    .tx_pop_i(dai_pop_i),
    .tx_ack_o(dai_ack_o),

    .data_i(cpu_data_o),
    .data_o(dai_tx_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

wire [31:0] debug_data_o;
nkmd_debug debug(
    .clk(clk), .rst(rst),

    .debug_led(debug_led), .switch(switch),

    .data_i(cpu_data_o),
    .data_o(debug_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

assign cpu_data_i =
    ram_data_o |
    uart_data_o |
    dai_rx_data_o | dai_tx_data_o |
    debug_data_o;

nkmd_progrom rom(
    .clk(clk),

    .addr_i(cpu_prog_addr_o),
    .data_o(cpu_prog_data_i));

endmodule
