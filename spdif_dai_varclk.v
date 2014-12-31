module spdif_dai_varclk #(
    parameter MIN_CLK_PER_HALFBIT = 4,
	parameter MAX_CLK_PER_HALFBIT = 31,
    parameter MAX_CLK_PER_HALFBIT_LOG2 = 5, // 32 max

    parameter NUM_RATE = 5
)(
    input clk,
    input rst,

    input signal_i,

    output [23:0] data_o,
    output ack_o,
    output locked_o,
    output rst_o,
    output lrck_o,
    output [191:0] udata_o,
    output [191:0] cdata_o,
    output [(NUM_RATE-1):0] rate_o);

wire [(MAX_CLK_PER_HALFBIT_LOG2-1):0] clk_per_halfbit;
wire locked;
spdif_dai #(
	.MAX_CLK_PER_HALFBIT_LOG2(MAX_CLK_PER_HALFBIT_LOG2)
) dai(
    .clk(clk),
    .rst(rst),

	.clk_per_halfbit(clk_per_halfbit),
    .signal_i(signal_i),

    .data_o(data_o),
    .ack_o(ack_o),
    .locked_o(locked),
    .lrck_o(lrck_o),
    .udata_o(udata_o),
    .cdata_o(cdata_o));

assign locked_o = locked;
reg last_locked_ff;
always @(posedge clk)
    last_locked_ff <= locked;
assign rst_o = !last_locked_ff && locked;

reg [11:0] unlocked_duration_counter;
wire unlocked_for_longtime = unlocked_duration_counter == 12'hfff;
always @(posedge clk) begin
	if(rst || locked)
		unlocked_duration_counter <= 0;
	else if(!locked)
		unlocked_duration_counter <= unlocked_duration_counter + 1;
end

reg [(MAX_CLK_PER_HALFBIT_LOG2-1):0] clk_per_halfbit_ff;
always @(posedge clk) begin
	if(rst)
		clk_per_halfbit_ff <= MIN_CLK_PER_HALFBIT;
	else if(unlocked_for_longtime || (last_locked_ff && !locked)) begin
		if(clk_per_halfbit_ff == MAX_CLK_PER_HALFBIT-1)
			clk_per_halfbit_ff <= MIN_CLK_PER_HALFBIT;
		else
			clk_per_halfbit_ff <= clk_per_halfbit + 1;
	end
end
assign clk_per_halfbit = clk_per_halfbit_ff;

// clk_per_halfbit -> sampling rate
parameter RATE_32 = 0;
parameter RATE_441 = 1;
parameter RATE_48 = 2;
parameter RATE_96 = 3;
parameter RATE_192 = 4;
reg [(NUM_RATE):0] rate_ff;
always @(posedge clk) begin
    case (clk_per_halfbit)
        5'd4, // rate: 192.0kHz
        5'd5, // rate: 153.6kHz
        5'd6: // rate: 128.0kHz
            rate_ff <= 5'b1 << RATE_192;
        5'd7, // rate: 109.7kHz
        5'd8, // rate: 96.0kHz
        5'd9, // rate: 85.3kHz
        5'd10: // rate: 76.8kHz
            rate_ff <= 5'b1 << RATE_96;
        5'd11, // rate: 69.8kHz
        5'd12, // rate: 64.0kHz
        5'd13, // rate: 59.1kHz
        5'd14, // rate: 54.9kHz
        5'd15, // rate: 51.2kHz
        5'd16: // rate: 48.0kHz
            rate_ff <= 5'b1 << RATE_48;
        5'd17, // rate: 45.2kHz
        5'd18, // rate: 42.7kHz
        5'd19, // rate: 40.4kHz
        5'd20: // rate: 38.4kHz
            rate_ff <= 5'b1 << RATE_441;
        5'd21, // rate: 36.6kHz
        5'd22, // rate: 34.9kHz
        5'd23, // rate: 33.4kHz
        5'd24, // rate: 32.0kHz
        5'd25, // rate: 30.7kHz
        5'd26, // rate: 29.5kHz
        5'd27, // rate: 28.4kHz
        5'd28, // rate: 27.4kHz
        5'd29, // rate: 26.5kHz
        5'd30, // rate: 25.6kHz
        5'd31: // rate: 24.8kHz
            rate_ff <= 5'b1 << RATE_32;
        default:
            rate_ff <= 0;
    endcase
end
assign rate_o = rate_ff;

endmodule
