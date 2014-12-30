module resample_pipeline
#(
    parameter NUM_CH = 8,
    parameter NUM_CH_LOG2 = 3,

    parameter NUM_RATE = 5
)(
    input clk,
    input rst,

    // data in
    input [(NUM_RATE*NUM_CH-1):0] rate_i,
    input [(NUM_CH-1):0] ack_i,
    input [(24*NUM_CH-1):0] data_i,
    output [(NUM_CH-1):0] pop_o,

    // data out
    input [(NUM_CH-1):0] pop_i,
    output [(24*NUM_CH-1):0] data_o,
    output [(NUM_CH-1):0] ack_o);

parameter RATE_32 = 0;
parameter RATE_441 = 1;
parameter RATE_48 = 2;
parameter RATE_96 = 3;
parameter RATE_192 = 4;

// 48kHz -> 96kHz upsampler
// INPUT:
wire [(NUM_CH-1):0] pop_441_480;
// OUTPUT:
wire [(24*NUM_CH-1):0] data_441_480;
wire [(NUM_CH-1):0] ack_441_480;

// 48kHz muxer
// OUTPUT:
wire [(24*NUM_CH-1):0] data_48;
wire [(NUM_CH-1):0] ack_48;

genvar ig48;
generate
for (ig48 = 0; ig48 < NUM_CH; ig48 = ig48 + 1) begin:g48
    wire in48 = rate_i[NUM_RATE*ig48+RATE_48];
    assign data_48[(24*ig48) +: 24] = in48 ? data_i[24*ig48 +: 24] : data_441_480;
    assign ack_48[ig48] = in48 ? ack_i[ig48] : ack_441_480;
end
endgenerate

// 48kHz -> 96kHz upsampler
// INPUT:
wire [(NUM_CH-1):0] pop_i_48_96;
// OUTPUT:
wire [(NUM_CH-1):0] pop_o_48_96;
wire [23:0] data_48_96;
wire [(NUM_CH-1):0] ack_48_96;

wire [23:0] bank_data_48_96;
wire [4:0] bank_addr_48_96;
rom_firbank_48_96 bank_48_96(.clk(clk), .addr(bank_addr_48_96), .data(bank_data_48_96));
ringbuffered_resampler #(
    .NUM_CH(NUM_CH), .NUM_CH_LOG2(NUM_CH_LOG2),
    .HALFDEPTH(16), .HALFDEPTH_LOG2(4),
    .NUM_FIR(2), .NUM_FIR_LOG2(1), .DECIM(1),
    .TIMESLICE(64), .TIMESLICE_LOG2(6)) resampler_48_96(
    .clk(clk), .rst(rst),
    .bank_addr_o(bank_addr_48_96), .bank_data_i(bank_data_48_96),
    .ack_i(ack_48), .data_i(data_48), .pop_o(pop_o_48_96),
    .pop_i(pop_i_48_96), .data_o(data_48_96), .ack_o(ack_48_96));

// 96kHz muxer
// OUTPUT: 
wire [(24*NUM_CH-1):0] data_96;
wire [(NUM_CH-1):0] ack_96;

genvar ig96;
generate
for (ig96 = 0; ig96 < NUM_CH; ig96 = ig96 + 1) begin:g96
    wire in96 = rate_i[NUM_RATE*ig96+RATE_96];
    assign data_96[(24*ig96) +: 24] = in96 ? data_i[24*ig96 +: 24] : data_48_96;
    assign ack_96[ig96] = in96 ? ack_i[ig96] : ack_48_96[ig96];
end
endgenerate

// 96kHz -> 192kHz upsampler
// INPUT:
wire [(NUM_CH-1):0] pop_i_96_192;
// OUTPUT:
wire [(NUM_CH-1):0] pop_o_96_192;
wire [23:0] data_96_192;
wire [(NUM_CH-1):0] ack_96_192;

wire [23:0] bank_data_96_192;
wire [3:0] bank_addr_96_192;
rom_firbank_96_192 bank_96_192(.clk(clk), .addr(bank_addr_96_192), .data(bank_data_96_192));
ringbuffered_resampler #(
    .NUM_CH(NUM_CH), .NUM_CH_LOG2(NUM_CH_LOG2),
    .HALFDEPTH(8), .HALFDEPTH_LOG2(3),
    .NUM_FIR(2), .NUM_FIR_LOG2(1), .DECIM(1),
    .TIMESLICE(32), .TIMESLICE_LOG2(5)) resampler_96_192(
    .clk(clk), .rst(rst),
    .bank_addr_o(bank_addr_96_192), .bank_data_i(bank_data_96_192),
    .ack_i(ack_96), .data_i(data_96), .pop_o(pop_o_96_192),
    .pop_i(pop_i_96_192), .data_o(data_96_192), .ack_o(ack_96_192));
assign pop_i_48_96 = pop_o_96_192;

// 192kHz muxer
genvar ig192;
generate
for (ig192 = 0; ig192 < NUM_CH; ig192 = ig192 + 1) begin:g192
    wire in192 = rate_i[NUM_RATE*ig192 + RATE_192];

    wire [23:0] data_192 = in192 ? data_i[24*ig192 +: 24] : data_96_192;
    wire ack_192 = in192 ? ack_i[ig192] : ack_96_192[ig192];
    wire pop_192 = pop_i[ig192];

    assign pop_i_96_192[ig192] = pop_192;

    // ringbuf
    wire [23:0] data_rb;
    ringbuf rb192(
        .clk(clk), .rst(rst),
        .data_i(data_192), .we_i(ack_192),
        .pop_i(pop_192), .offset_i(4'b0), .data_o(data_rb));
    reg ack_rb_ff;
    always @(posedge clk) begin
        if (rst)
            ack_rb_ff <= 0;
        else
            ack_rb_ff <= pop_192;
    end
    assign data_o[(24*ig192) +: 24] = data_rb;
    assign ack_o[ig192] = ack_rb_ff;
end
endgenerate

// pop_o sel
reg [(NUM_CH-1):0] pop_o_reg; // combinatory logic
genvar igpop;
generate
for (igpop = 0; igpop < NUM_CH; igpop = igpop + 1) begin:gpop
    wire [(NUM_RATE-1):0] pop_rate = rate_i[NUM_RATE*igpop +: NUM_RATE];
    always @(*) begin
        if (pop_rate[RATE_192])
            pop_o_reg[igpop] = pop_i[igpop];
        else if (pop_rate[RATE_96])
            pop_o_reg[igpop] = pop_o_96_192[igpop];
        else if (pop_rate[RATE_48])
            pop_o_reg[igpop] = pop_o_48_96[igpop];
        // FIXME: add more
    end
end
endgenerate
assign pop_o = pop_o_reg;

endmodule
