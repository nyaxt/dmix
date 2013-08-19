module spdif_dai_varclk #(
	parameter CLK_PER_BIT = 8,
	parameter CLK_PER_BIT_LOG2 = 3
)(
    input clk903168,
    input clk983040,
    input rst,

    input signal_i,

    output [23:0] data_o,
    output wpulse_o,
    output locked_o,
    output lrck_o,
    output [191:0] udata_o,
    output [191:0] cdata_o,
    output [3:0] rate_o);

wire ack;
reg [3:0] pulse_counter;
always @(posedge clk983040) begin
    if(ack) begin
        pulse_counter <= 4'hf;
    end else if(pulse_counter > 0) begin
        pulse_counter <= pulse_counter - 1;
    end
end
assign wpulse_o = pulse_counter > 0;

spdif_dai dai(
    .clk(clk983040),
    .rst(rst),

    .signal_i(signal_i),

    .data_o(data_o),
    .ack_o(ack),
    .locked_o(locked_o),
    .lrck_o(lrck_o),
    .udata_o(udata_o),
    .cdata_o(cdata_o));
assign rate_o = 4'b0010; // 48k

endmodule
