// 8-N-1: 8bit no parity 1 stop bit
module uart(
    input wire clk,

    input wire rx,
    output wire tx,

    output wire [7:0] data_o,
    output wire ack_o,

    input wire [7:0] data_i,
    input wire ack_i,
    output wire pop_o);

// trig_baud16 is 1 at 9600 * 16Hz = 153600Hz
// 50MHz / 153600Hz = 325.52
wire trig_baud16;
wire trig_baud;
`define UART_DIV 11'd325

reg [10:0] bauddiv_counter_ff;
`ifdef SIMULATION
initial bauddiv_counter_ff = 11'd567; // random
`endif
always @(posedge clk) begin
    if (bauddiv_counter_ff == 0) begin
        bauddiv_counter_ff <= `UART_DIV;
    end else begin
        bauddiv_counter_ff <= bauddiv_counter_ff - 1;
    end
end
assign trig_baud16 = (bauddiv_counter_ff == 4'd0);

reg [3:0] baudrate_counter_ff;
`ifdef SIMULATION
initial baudrate_counter_ff = 4'd14; // random
`endif
always @(posedge clk) begin
    if (trig_baud16)
        baudrate_counter_ff <= baudrate_counter_ff + 1;
end
reg trig_baud_ff;
always @(posedge clk) begin
    trig_baud_ff <= trig_baud16 && (baudrate_counter_ff == 4'd0);
end
assign trig_baud = trig_baud_ff;

// rx
reg [1:0] rx_hist_ff;
`ifdef SIMULATION
initial rx_hist_ff = 4'd3; // random
`endif
always @(posedge clk)
    rx_hist_ff <= {rx_hist_ff[0], rx};

wire rx_posedge = rx_hist_ff[1:0] == 2'b01;
wire rx_negedge = rx_hist_ff[1:0] == 2'b10;

reg [4:0] rx_counter_ff;
reg [4:0] rx_poscounter_ff;
`ifdef SIMULATION
initial rx_counter_ff = 4'd3; // random
initial rx_counter_ff = 4'd10; // random
`endif
wire trig_next_uartbit = (rx_counter_ff == 5'd17) || rx_negedge || rx_posedge;

always @(posedge clk) begin
    if (trig_next_uartbit) begin
        rx_counter_ff <= 0;
        rx_poscounter_ff <= 0;
    end else begin
        if (trig_baud16) begin
            rx_counter_ff <= rx_counter_ff + 1;
            if (rx_hist_ff[0] == 1'b1)
                rx_poscounter_ff <= rx_poscounter_ff + 1;
        end
    end
end

wire currbit = rx_poscounter_ff > 8;
reg [7:0] rxbuf_ff;
always @(posedge clk) begin
    if (trig_next_uartbit)
        rxbuf_ff <= {currbit, rxbuf_ff[7:1]};
end

assign data_o = rxbuf_ff;

reg [3:0] rx_frame_counter_ff;
`ifdef SIMULATION
initial rx_frame_counter_ff = 4'd12; // random
`endif
always @(posedge clk) begin
    if (trig_next_uartbit) begin
        if (rx_frame_counter_ff > 4'd8) begin
            if (rx_negedge) begin
                rx_frame_counter_ff <= 0;
            end
        end else begin
            rx_frame_counter_ff <= rx_frame_counter_ff + 1;
        end
    end
end

reg rx_pop_ff;
always @(posedge clk) begin
    rx_pop_ff <= trig_next_uartbit && rx_frame_counter_ff == 4'd8;
end
assign ack_o = rx_pop_ff;

// tx 
reg [3:0] tx_frame_counter_ff;
`ifdef SIMULATION
initial tx_frame_counter_ff = 4'd13; // random
`endif
always @(posedge clk) begin
    if (ack_i) begin
        tx_frame_counter_ff <= 0;
    end else if (trig_baud) begin
        tx_frame_counter_ff <= tx_frame_counter_ff + 1;
    end
end
assign pop_o = tx_frame_counter_ff > 4'd9;

reg [10:0] txbuf_ff;
always @(posedge clk) begin
    if (ack_i) begin
        //           stop          start, preserve H until next trig_baud
        txbuf_ff <= {1'b1, data_i,  1'b0, 1'b1};
    end else if (trig_baud) begin
        txbuf_ff <= {1'b1, txbuf_ff[9:1]};
    end
end
assign tx = txbuf_ff[0];

endmodule
