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
) dai_fixedclk(
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

reg [13:0] unlocked_duration_counter;
wire unlocked_for_longtime = unlocked_duration_counter == 14'h3fff;
always @(posedge clk) begin
	if(rst || locked)
		unlocked_duration_counter <= 0;
	else if(!locked)
		unlocked_duration_counter <= unlocked_duration_counter + 1;
end

reg [(NUM_RATE-1):0] rate_ff;
parameter RATE_32 = 0;
parameter RATE_441 = 1;
parameter RATE_48 = 2;
parameter RATE_96 = 3;
parameter RATE_192 = 4;
always @(posedge clk) begin
	if (rst)
		rate_ff <= 5'd1 << RATE_192;
	else if(unlocked_for_longtime || (last_locked_ff && !locked)) begin
		if(rate_ff == (5'd1 << RATE_32))
            rate_ff <= 5'd1 << RATE_192;
		else
			rate_ff <= {1'b0, rate_ff[(NUM_RATE-1):1]}; // shr
	end
end
assign rate_o = rate_ff;

reg [(MAX_CLK_PER_HALFBIT_LOG2-1):0] clk_per_halfbit_ff;
always @(posedge clk) begin
    // sampling rate -> clk_per_halfbit
    case (rate_ff)
        5'd1 << RATE_32:
            clk_per_halfbit_ff <= 5'd24;
        5'd1 << RATE_441:
            clk_per_halfbit_ff <= 5'd18; // or d18???
        5'd1 << RATE_48:
            clk_per_halfbit_ff <= 5'd16;
        5'd1 << RATE_96:
            clk_per_halfbit_ff <= 5'd8;
        5'd1 << RATE_192:
            clk_per_halfbit_ff <= 5'd4;
        default:
            clk_per_halfbit_ff <= 5'd4;
    endcase
end
assign clk_per_halfbit = clk_per_halfbit_ff;

endmodule
