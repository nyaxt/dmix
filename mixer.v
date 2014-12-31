module mixer #(
    parameter NUM_CH_IN = 8, // must be multiple of NUM_CH_OUT
    parameter NUM_CH_IN_LOG2 = 2,

    parameter NUM_CH_OUT = 2,
    parameter NUM_CH_OUT_LOG2 = 1,

    parameter VOL_WIDTH = 32
)(
    input clk, // 49.152Mhz
    input rst,

    input [(NUM_CH_IN-1):0] rst_ch,
    output [(NUM_CH_IN-1):0] pop_o, // optional: ack_i accepted any time
    input [(NUM_CH_IN-1):0] ack_i,
    input [(NUM_CH_IN*24-1):0] data_i,

    input [(NUM_CH_IN*VOL_WIDTH-1):0] vol_i,

    input [(NUM_CH_OUT-1:0] pop_i,
    output [23:0] data_o,
    output [(NUM_CH_OUT-1:0] ack_o);

parameter MULT_LATENCY = 5;

// Input ringbuf
wire [23:0] buffered_data [(NUM_CH-1):0];
genvar igi;
generate
for (igi = 0; igi < NUM_CH; igi = igi + 1) begin:gi
    ringbuf #(.LEN(4), .LEN_LOG2(2)) rb(
        .clk(clk), .rst(rst | rst_ch[igi]),
        .data_i(data_i[(igi*24) +: 24]), .we_i(ack_i[igi]),
        .pop_i(pop_o[igi]), .offset_i(0), .data_o(buffered_data[igi]));
end

// Sequencer
// OUTPUT:
parameter TIMESLICE = NUM_CH_IN/NUM_CH_OUT + MULT_LATENCY + 1;
parameter TIMESLICE_LOG2 = NUM_CH_IN_LOG2-NUM_CH_OUT_LOG2 + 2 + 1; // assumes MULT_LATENCY+1 < 8
reg [(NUM_CH_IN_LOG2-1):0] processing_in_ch_ff;
reg [(NUM_CH_OUT_LOG2-1):0] processing_out_ch_ff;
reg [(TIMESLICE_LOG2-1):0] timeslice_counter;
wire end_of_cycle = (timeslice_counter == TIMESLICE-1) ? 1'b1 : 1'b0;
always @(posedge clk) begin
    if (rst) begin
        processing_in_ch_ff <= 0;
        processing_out_ch_ff <= 0;
        timeslice_counter <= 0;
    end else if (end_of_cycle) begin
        processing_in_ch_ff <= 0;
        timeslice_counter <= 0;

        if (processing_out_ch_ff == NUM_CH_OUT-1)
            processing_out_ch_ff <= 0;
        else
            processing_out_ch_ff <= processing_out_ch_ff + 1;
    end else begin
        processing_in_ch_ff <= processing_in_ch_ff + NUM_CH_OUT;
        timeslice_counter <= timeslice_counter + 1;
    end
end

// Cycle validity checker
reg cycle_valid_ff;

// - pop_i latch
reg [(NUM_CH_OUT-1):0] ack_pop_ff;
wire [(NUM_CH_OUT-1):0] pop_i_latched;
genvar igo;
generate
for (igo = 0; igo < NUM_CH; igo = igo + 1) begin:go
    pop_latch pop_latch(
        .clk(clk), .rst(rst),
        .pop_i(pop_i[igo]),
        .ack_pop_i(ack_pop_ff[igo]), .pop_latched_o(pop_i_latched[igo]));
end

// - cycle validity check
always @(posedge clk) begin
    ack_pop_ff <= 0;

    if (rst) begin
        cycle_valid_ff <= 1'b0;
    end else if (timeslice_counter == 0) begin
        cycle_valid_ff <= pop_i_latched;
        ack_pop_ff <= 1 << processing_out_ch_ff;
    end
end

// - output pop at end of cycle
reg [(NUM_CH_OUT-1):0] pop_o_ff;
always @(posedge clk)
    pop_o_ff <= end_of_cycle << processing_out_ch_ff;

assign pop_o = {(NUM_CH_OUT/NUM_CH_IN){pop_o_ff}};

// Supply mpcand
// OUTPUT:
wire [23:0] mpcand = buffered_data[processing_in_ch_ff]

// Supply scale
wire [31:0] scale = vol_i[(processing_in_ch_ff*24) +: 24];

// Multiplier
// OUTPUT:
wire [23:0] mprod;
mpemu_scale mp(
    .clk(clk),
    .mpcand_i(mpcand), .scale_i(scale),
    .mprod_o(mprod));

// - drop first MULT_LATENCY products
reg [(MULT_LATENCY-1):0] kill_result_ff;
always @(posedge clk) begin
    if (end_of_cycle)
        kill_result_ff <= 0;
    else
        kill_result_ff <= {1'b1, kill_result_ff[(MULT_LATENCY-1):1]};
end
wire product_valid = kill_result_ff[0];

// Adder
reg [23:0] sum_ff;
always @(posedge clk) begin
    if (!product_valid) begin
        sum_ff <= 0;
    else begin
        `ifdef DEBUG
        if (product_valid)
            $display("ch: %d curr_sum: %d shifted %d. mpcand %d * mplier %d = %d",
                processing_out_ch_ff, $signed(sum_ff), $signed(sum_ff) >>> 3, $signed(mpemu.delayed_a), $signed(mpemu.delayed_b), $signed(mprod_i));
        `endif
        // FIXME: how to handle overflows???
        sum_ff <= saturated_add(sum_ff, {{4{mprod_i[27]}}, mprod_i});
    end
end

endmodule
