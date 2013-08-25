module spdif_dai_varclk #(
    parameter MIN_CLK_PER_HALFBIT = 4,
	parameter MAX_CLK_PER_HALFBIT = 31,
    parameter MAX_CLK_PER_HALFBIT_LOG2 = 5 // 32 max
)(
    input clk,
    input rst,

    input signal_i,

    output [23:0] data_o,
    output ack_o,
    output locked_o,
    output lrck_o,
    output [191:0] udata_o,
    output [191:0] cdata_o,
    output [3:0] rate_o);

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
assign rate_o = 4'b0010; // 48k
assign locked_o = locked;

reg [11:0] unlocked_duration_counter;
wire unlocked_for_longtime = unlocked_duration_counter == 12'hfff;
always @(posedge clk) begin
	if(rst || locked)
		unlocked_duration_counter <= 0;
	else if(!locked)
		unlocked_duration_counter <= unlocked_duration_counter + 1;
end

reg [(MAX_CLK_PER_HALFBIT_LOG2-1):0] clk_per_halfbit;
always @(posedge clk) begin
	if(rst)
		clk_per_halfbit <= MIN_CLK_PER_HALFBIT;
	else if(unlocked_for_longtime) begin
		if(clk_per_halfbit == MAX_CLK_PER_HALFBIT-1)
			clk_per_halfbit <= MIN_CLK_PER_HALFBIT;
		else
			clk_per_halfbit <= clk_per_halfbit + 1;
	end
end

endmodule
