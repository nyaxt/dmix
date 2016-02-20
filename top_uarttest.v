module uarttest_top(
    input clk_50mhz,

    input rx,
    output tx,

    output [7:0] debug_led,
    input [7:0] switch,
    input [3:0] button);

wire [7:0] uart_data_o;
wire uart_ack_o;
uart uart(
    .clk(clk_50mhz),
    .rx(rx), .tx(tx),
    .data_o(uart_data_o), .ack_o(uart_ack_o),
    .data_i(switch), .ack_i(button[0]));

reg [7:0] rxbuf_ff;
always @(posedge clk_50mhz) begin
    if (uart_ack_o)
        rxbuf_ff <= uart_data_o;
end
assign debug_led[7:0] = rxbuf_ff[7:0];

endmodule
