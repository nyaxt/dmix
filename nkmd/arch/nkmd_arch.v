module nkmd_arch #(
    parameter NKMDDBG_WIDTH = 16*8
)(
    input wire clk,
    input wire rst,
    
    input wire uart_rx,
    output wire uart_tx,

    input wire [23:0] dai_data_i,
    input wire dai_ack_i,

    output wire [23:0] dai_data_o,
    input wire dai_pop_i,
    output wire dai_ack_o,

`ifdef PROMW
    input wire [31:0] prog_addr_i,
    input wire [31:0] prog_data_i,
    input wire prog_ack_i,
`endif

    output wire [(NKMDDBG_WIDTH-1):0] dbgout_o,
    input wire [(NKMDDBG_WIDTH-1):0] dbgin_i);

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

    .dbgout_o(dbgout_o), .dbgin_i(dbgin_i),

    .data_i(cpu_data_o),
    .data_o(debug_data_o),
    .addr_i(cpu_addr_o),
    .we_i(cpu_we_o));

assign cpu_data_i =
    ram_data_o |
    uart_data_o |
    dai_rx_data_o | dai_tx_data_o |
    debug_data_o;

`ifdef PROMW
nkmd_progrom_w
`else
nkmd_progrom
`endif
    rom(
    .clk(clk),

    .addr_i(cpu_prog_addr_o),
    .data_o(cpu_prog_data_i)

`ifdef PROMW
    ,
    .prog_addr_i(prog_addr_i),
    .prog_data_i(prog_data_i),
    .prog_ack_i(prog_ack_i)
`endif
);

endmodule
