module resample_pipeline(
    input clk,
    input rst,

    // input rate
    input [3:0] rate_i,

    // data input
    output [1:0] pop_o,
    input [23:0] data_i,
    input [1:0] ack_i,

    // 192k output
    input [1:0] pop_i,
    output [23:0] data_o,
    output [1:0] ack_o);

parameter RATE_441 = 0;
parameter RATE_48 = 1;
parameter RATE_96 = 2;
parameter RATE_192 = 3;

parameter SCHED_SLOT = 40;
reg [2:0] mpready_shift;
reg [6:0] mpsched_counter;
always @(posedge clk) begin
	if(pop_i) begin
		mpsched_counter <= 0;
		mpready_shift <= 3'b001;
	end else if(mpsched_counter == SCHED_SLOT-1) begin
		mpsched_counter <= 0;
		mpready_shift <= {mpready_shift[1:0], 1'b0};
	end else
		mpsched_counter <= mpsched_counter + 1;
end
wire mpready_441to48 = mpready_shift[2];
wire mpready_48to96 = mpready_shift[1];
wire mpready_96to192 = mpready_shift[0];

// multiplier
wire [23:0] mpcand441;
wire [23:0] mpcand48;
wire [23:0] mpcand96;
wire [15:0] mplier441;
wire [15:0] mplier48;
wire [15:0] mplier96;

wire [23:0] mpcand = mpcand441 | mpcand48 | mpcand96;
wire [15:0] mplier = mplier441 | mplier48 | mplier96;
wire [23:0] mprod;
mpemu mpemu(
    .clk(clk),
    .mpcand_i(mpcand), .mplier_i(mplier), .mprod_o(mprod));

wire [23:0] data_441 = data_i;
wire [1:0] ack_441 = ack_i;
wire [1:0] pop_48;

wire [1:0] pop_o_441;
wire [23:0] data_o_48;
wire [1:0] ack_o_48;
resample441_48 resample441_48(
    .clk(clk), .rst(rst),
    .mpready_i(mpready_441to48), .mpcand_o(mpcand441), .mplier_o(mplier441), .mprod_i(mprod),
    .pop_o(pop_o_441), .data_i(data_441), .ack_i(ack_441),
    .pop_i(pop_48), .data_o(data_o_48), .ack_o(ack_o_48));

wire [23:0] data_48 = rate_i[RATE_48] ? data_i : data_o_48;
wire [1:0] ack_48 = rate_i[RATE_48] ? ack_i : ack_o_48;
wire [1:0] pop_96;

wire [1:0] pop_o_48;
wire [23:0] data_o_96;
wire [1:0] ack_o_96;
upsample2x ups_48to96(
    .clk(clk), .rst(rst),
    .mpready_i(mpready_48to96), .mpcand_o(mpcand48), .mplier_o(mplier48), .mprod_i(mprod),
    .pop_o(pop_o_48), .data_i(data_48), .ack_i(ack_48),
    .pop_i(pop_96), .data_o(data_o_96), .ack_o(ack_o_96));

wire [23:0] data_96 = rate_i[RATE_96] ? data_i : data_o_96;
wire [1:0] ack_96 = rate_i[RATE_96] ? ack_i : ack_o_96;

wire [1:0] pop_o_96;
wire [23:0] data_o_192;
wire [1:0] ack_o_192;
upsample2x ups_96to192(
    .clk(clk), .rst(rst),
    .mpready_i(mpready_96to192), .mpcand_o(mpcand96), .mplier_o(mplier96), .mprod_i(mprod),
    .pop_o(pop_o_96), .data_i(data_96), .ack_i(ack_96),
    .pop_i(pop_i), .data_o(data_o_192), .ack_o(ack_o_192));

assign data_o = rate_i[RATE_192] ? data_i : data_o_192;
assign ack_o = rate_i[RATE_192] ? ack_i : ack_o_192;

// FIXME: use ringbuf even when 192k
reg [1:0] pop_reg;
always @(rate_i or pop_i or pop_o_96 or pop_o_48 or pop_o_441) begin
    if(rate_i[RATE_192])
        pop_reg = pop_i;
    else if (rate_i[RATE_96])
        pop_reg = pop_o_96;
    else if (rate_i[RATE_48])
        pop_reg = pop_o_48;
    else // if (pop_i[RATE_441])
        pop_reg = pop_o_441;
end
assign pop_o = pop_reg;
assign pop_96 = rate_i[RATE_96] ? pop_i : pop_o_96;
assign pop_48 = rate_i[RATE_48] ? pop_i : pop_o_48;

endmodule
