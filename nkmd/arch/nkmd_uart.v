module nkmd_uart(
    input clk,
    input rst,

    input rx,
    output tx,

    input [31:0] data_i,
    output [31:0] data_o,
    input [31:0] addr_i,
    input we_i);

wire [7:0] uart_data_o;
wire uart_ack_o;

wire [7:0] uart_data_i;
wire uart_ack_i;
wire uart_pop_o;

uart uart(
    .clk(clk),

    .rx(rx),
    .tx(tx),

    .data_o(uart_data_o),
    .ack_o(uart_ack_o),

    .data_i(uart_data_i),
    .ack_i(uart_ack_i),
    .pop_o(uart_pop_o));

// tx
assign uart_data_i = data_i[7:0];
assign uart_ack_i = (addr_i == 16'hc001) && we_i;

reg uart_tx_ready_ff;
always @(posedge clk) begin
    if (rst || uart_ack_i) begin
        uart_tx_ready_ff <= 1'b0;
    end else if (uart_pop_o) begin
        uart_tx_ready_ff <= 1'b1;
    end
end

// rx
reg uart_rx_ready_ff;
reg [7:0] uart_rx_buf_ff;
always @(posedge clk) begin
    if (rst) begin
        uart_rx_ready_ff <= 1'b0;
        uart_rx_buf_ff <= 8'h00;
    end else begin
        if (addr_i == 16'hc002) begin
            uart_rx_ready_ff <= 1'b0;
        end

        if (uart_ack_o) begin
            uart_rx_ready_ff <= 1'b1;
            uart_rx_buf_ff <= uart_data_o;
        end
    end
end

reg [7:0] data_o_ff;
always @(posedge clk) begin
    case (addr_i)
    16'hc002:
        data_o_ff <= uart_data_o;
    16'hc003:
        data_o_ff <= {7'b0, uart_tx_ready_ff};
    16'hc004:
        data_o_ff <= {7'b0, uart_rx_ready_ff};
    default:
        data_o_ff <= 8'b0;
endcase
end
assign data_o[31:8] = 0;
assign data_o[7:0] = data_o_ff;

endmodule
