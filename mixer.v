module mixer #(
parameter NUM_CH = 2,
parameter NUM_CH_LOG2 = 1,
parameter VOL_WIDTH = 32
)(
    input clk, // 49.152Mhz
    input rst,
    input [(NUM_CH-1):0] rst_ch,

    output [(NUM_CH-1):0] pop_o,
    input [(NUM_CH-1):0] ack_i,
    input [(NUM_CH*24-1):0] data_i,

    input [(NUM_CH*VOL_WIDTH-1):0] vol_i,

    input [1:0] pop_i,
    output [23:0] data_o,
    output [1:0] ack_o);

wire [23:0] buffered_data [(NUM_CH-1):0];

reg [1:0] pop_o_ff;

assign pop_o = {NUM_CH{pop_o_ff}};
genvar ig;
generate
for (ig = 0; ig < NUM_CH; ig = ig + 1) begin:g
    ringbuf rb(
        .clk(clk), .rst(rst | rst_ch[ig]),
        .data_i(data_i[(ig*24) +: 24]), .we_i(ack_i[ig]),
        .pop_i(pop_o_ff), .offset_i(0), .data_o(buffered_data[ig]));
end

always @(posedge clk) begin
    
end

endmodule
