module uart(
    input clk,

    input rx,
    output tx,

    output [7:0] data_o,
    output ack_pop_o,

    input [7:0] data_i,
    input ack_i);

// trig_baud16 is 1 at 9600 * 16Hz = 153600Hz
// 50MHz / 153600Hz = 325.52
wire trig_baud16;
`define UART_DIV 11'd325

reg [10:0] counter_ff;
`ifdef SIMULATION
initial counter_ff = 11'd567; // random
`endif
always @(posedge clk) begin
    if (counter_ff == 0) begin
        counter_ff <= `UART_DIV;
    end else begin
        counter_ff <= counter_ff - 1;
    end
end

assign trig_baud16 = (counter_ff == 0);

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

reg [3:0] frame_counter_ff;
`ifdef SIMULATION
initial frame_counter_ff = 4'd12; // random
`endif
always @(posedge clk) begin
    if (trig_next_uartbit) begin
        if (frame_counter_ff > 4'd8) begin
            if (rx_negedge) begin
                frame_counter_ff <= 0;
            end
        end else begin
            frame_counter_ff <= frame_counter_ff + 1;
        end
    end
end

reg pop_ff;
always @(posedge clk) begin
    pop_ff <= trig_next_uartbit && frame_counter_ff == 4'd8;
end
assign ack_pop_o = pop_ff;

endmodule
