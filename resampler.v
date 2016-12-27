// `define DEBUG

module resampler_core
#(
    parameter NUM_CH = 8,
    parameter NUM_CH_LOG2 = 3,

    parameter HALFDEPTH = 16,
    parameter HALFDEPTH_LOG2 = 4,
    parameter NUM_FIR = 160,
    parameter NUM_FIR_LOG2 = 8,
    parameter DECIM = 147,
    parameter BANK_WIDTH = NUM_FIR_LOG2+HALFDEPTH_LOG2,

    parameter TIMESLICE = 64,
    parameter TIMESLICE_LOG2 = 6
)(
    input wire clk,
    input wire rst,

    // to firbank
    output wire [(BANK_WIDTH-1):0] bank_addr_o,
    input wire [23:0] bank_data_i,

    // to ringbuf array
    output wire [(NUM_CH-1):0] pop_o,
    output wire [(HALFDEPTH_LOG2+1-1):0] offset_o,
    input wire [(NUM_CH*24-1):0] data_i,

    // data output wire
    input wire [(NUM_CH-1):0] pop_i,
    output wire [23:0] data_o,
    output wire [(NUM_CH-1):0] ack_o);

// Latch pop_i request
wire [(NUM_CH-1):0] ack_pop_i;
wire [(NUM_CH-1):0] pop_i_latched;
genvar igl;
generate
for(igl = 0; igl < NUM_CH; igl = igl + 1) begin:g
    pop_latch pop_latch(
        .clk(clk), .rst(rst),
        .pop_i(pop_i[igl]),
        .ack_pop_i(ack_pop_i[igl]), .pop_latched_o(pop_i_latched[igl]));
end
endgenerate

// Fixed timeslice based scheduling and decide ch to process
reg [(NUM_CH_LOG2-1):0] processing_ch_ff;
reg rst_processing_ff;

reg [(TIMESLICE_LOG2-1):0] timeslice_counter;
wire timeslice_deadline = (timeslice_counter == TIMESLICE-1);

always @(posedge clk) begin
    if (rst) begin
        timeslice_counter <= 0;
        processing_ch_ff <= 0;
        rst_processing_ff <= 1;
    end else begin
        timeslice_counter <= timeslice_counter + 1;

        if (timeslice_deadline) begin
            timeslice_counter <= 0;
            processing_ch_ff <= processing_ch_ff + 1;
            rst_processing_ff <= 1;
        end else
            rst_processing_ff <= 0;
    end
end

// Sequence management
reg [7:0] state_ff; // Note: bit width will be optimized anyway.

parameter MULT_LATENCY = 5;
parameter ST_READY = 0;
parameter ST_MULADD_RWING = 2; // HALFDEPTH clk
parameter ST_MULADD_LWING = 3; // HALFDEPTH clk
parameter ST_WAIT_RESULT = 4;  // 1 + MULT_LATENCY clk
parameter ST_SATURATE = 5; // 1 clk
parameter ST_END_CYCLE = 6; // 1 clk
parameter ST_IDLE = 7;
reg [(NUM_CH-1):0] ack_pop_ff;
assign ack_pop_i = ack_pop_ff;

reg [HALFDEPTH_LOG2:0] muladd_wing_cycle_counter;

always @(posedge clk) begin
    if (rst_processing_ff) begin
        ack_pop_ff <= 1 << processing_ch_ff;
        state_ff <= ST_READY;
    end else begin
        muladd_wing_cycle_counter <= muladd_wing_cycle_counter + 1;

        ack_pop_ff <= 0;
        case (state_ff)
            ST_READY: begin
                if (pop_i_latched[processing_ch_ff]) begin
                    state_ff <= ST_MULADD_RWING;
                    muladd_wing_cycle_counter <= 0;
                end
            end
            ST_MULADD_RWING: begin
                if (muladd_wing_cycle_counter == HALFDEPTH-1) begin
                    state_ff <= ST_MULADD_LWING;
                    muladd_wing_cycle_counter <= 0;
                end
            end
            ST_MULADD_LWING: begin
                if (muladd_wing_cycle_counter == HALFDEPTH-1) begin
                    state_ff <= ST_WAIT_RESULT;
                    muladd_wing_cycle_counter <= 0;
                end
            end
            ST_WAIT_RESULT: begin
                if (muladd_wing_cycle_counter == 1+MULT_LATENCY-1)
                    state_ff <= ST_SATURATE;
            end
            ST_SATURATE: begin
                state_ff <= ST_END_CYCLE;
            end
            ST_END_CYCLE: begin
                state_ff <= ST_IDLE;
            end
            ST_IDLE: begin end // NOP
        endcase
    end
end

// Compute polyphase FIR filter index
// OUTPUT:
reg [(NUM_FIR_LOG2-1):0] firidx_rwing_currch_ff;
reg [(NUM_FIR_LOG2-1):0] firidx_lwing_currch_ff;

reg [(NUM_CH-1):0] pop_o_ff;
assign pop_o = pop_o_ff;

reg [(NUM_FIR_LOG2-1):0] firidx_mem [(NUM_CH-1):0];

integer i;
always @(posedge clk) begin
    pop_o_ff <= 0;

    if (rst) begin
        for (i = 0; i < NUM_CH; i = i + 1) begin
            firidx_mem[i] <= 0;
        end
        
        firidx_lwing_currch_ff <= 0;
    end else begin
        case (state_ff)
            ST_READY: begin
                firidx_lwing_currch_ff <= firidx_mem[processing_ch_ff];
                firidx_rwing_currch_ff <= NUM_FIR-1 - firidx_mem[processing_ch_ff];
            end
            ST_MULADD_RWING: begin
                `ifdef DEBUG
                if (muladd_wing_cycle_counter == 0)
                    $display("ch: %d. firidx_lwing: %d", processing_ch_ff, firidx_mem[processing_ch_ff]);
                `endif
            end
            ST_END_CYCLE: begin
                if (firidx_lwing_currch_ff >= NUM_FIR - DECIM) begin
                    firidx_mem[processing_ch_ff] <= firidx_lwing_currch_ff + DECIM - NUM_FIR;
                    pop_o_ff[processing_ch_ff] <= 1;
                    `ifdef DEBUG
                    $display("ch: %d. pop!", processing_ch_ff);
                    `endif
                end else begin
                    firidx_mem[processing_ch_ff] <= firidx_lwing_currch_ff + DECIM;
                end
            end
        endcase
    end 
end

// Supply mplier
// OUTPUT:
wire [23:0] mplier_o = bank_data_i;

wire mplier_lwing_active = state_ff == ST_MULADD_LWING ? 1'b1 : 1'b0;
wire [(NUM_FIR_LOG2-1):0] firidx = mplier_lwing_active ? firidx_lwing_currch_ff : firidx_rwing_currch_ff;
reg [(HALFDEPTH_LOG2-1):0] depthidx_ff;

assign bank_addr_o[(BANK_WIDTH-1):0] = {firidx, depthidx_ff}; // FIXME: HALFDEPTH must be power of 2
always @(posedge clk) begin
    case (state_ff)
    ST_READY:
        depthidx_ff <= HALFDEPTH-1;
    ST_MULADD_RWING:
        if (depthidx_ff != 0)
            depthidx_ff <= depthidx_ff - 1;
    ST_MULADD_LWING:
        depthidx_ff <= depthidx_ff + 1;
endcase
end

// Supply mpcand
// OUTPUT:
wire [23:0] mpcand_o;

reg [(HALFDEPTH_LOG2-1):0] offset_counter;
assign offset_o = {~mplier_lwing_active, offset_counter};
always @(posedge clk) begin
    offset_counter <= offset_counter - 1;
    if (state_ff == ST_READY)
        offset_counter <= HALFDEPTH-1;
end

wire [23:0] data_i_ary [(NUM_CH-1):0];
genvar ig;
generate
    for (ig = 0; ig < NUM_CH; ig = ig + 1) begin:data_i_to_ary
        assign data_i_ary[ig] = data_i[(ig*24)+:24];
    end
endgenerate
assign mpcand_o = data_i_ary[processing_ch_ff];

// Multiplier
wire [27:0] mprod_i;
mpemu mpemu(.clk(clk), .mpcand_i(mpcand_o), .mplier_i(mplier_o), .mprod_o(mprod_i));
parameter KILL_RESULT = MULT_LATENCY + 1; // 1 clk for rom/ringbuf latency
reg [(KILL_RESULT-1):0] kill_result_ff;
always @(posedge clk) begin
    if (state_ff == ST_READY)
        kill_result_ff <= 0;
    else
        kill_result_ff <= {1'b1, kill_result_ff[(KILL_RESULT-1):1]};
end
wire product_valid = kill_result_ff[0];

// Adder
reg [31:0] sum_ff;

function [31:0] saturated_add(
    input [31:0] a,
    input [31:0] b);

reg [32:0] aext;
reg [32:0] bext;
reg [32:0] sumext;

begin
    aext = {a[31], a};
    bext = {b[31], b};
    sumext = $signed(aext) + $signed(bext);

    case (sumext[32:31])
        2'b00, 2'b11: // sum is in expressible range
            saturated_add = sumext[31:0];
        2'b01: // overflow
            saturated_add = 32'h7fff_ffff;
        2'b10: // underflow
            saturated_add = 32'h8000_0000;
    endcase
end
endfunction

always @(posedge clk) begin
    if (!product_valid) begin
        sum_ff <= 0;
    end else begin
        `ifdef DEBUG
        if (ST_READY < state_ff && state_ff < ST_END_CYCLE)
            $display("ch: %d curr_sum: %d shifted %d. mpcand %d * mplier %d = %d",
                processing_ch_ff, $signed(sum_ff), $signed(sum_ff) >>> 3, $signed(mpemu.delayed_a), $signed(mpemu.delayed_b), $signed(mprod_i));
        `endif
        sum_ff <= saturated_add(sum_ff, {{4{mprod_i[27]}}, mprod_i});
    end
end

// Result
reg [23:0] saturated_sum_ff;
always @(posedge clk) begin
    if (sum_ff[31] == 1'b0) begin
        // sum +
        if (sum_ff[30:27] != 4'b0000)
            saturated_sum_ff <= 24'h7f_ffff; // overflow
        else
            saturated_sum_ff <= {1'b0, sum_ff[26:3]}; // sum is in expressible range
    end else begin
        // sum -
        if (sum_ff[30:27] != 4'b1111)
            saturated_sum_ff <= 24'h80_0000; // underflow
        else
            saturated_sum_ff <= {1'b1, sum_ff[26:3]}; // sum is in expressible range
    end
end
assign data_o = saturated_sum_ff;
assign ack_o = (state_ff == ST_END_CYCLE ? 1 : 0) << processing_ch_ff;

endmodule

module ringbuffered_resampler
#(
    parameter NUM_CH = 8,
    parameter NUM_CH_LOG2 = 3,

    parameter HALFDEPTH = 16,
    parameter HALFDEPTH_LOG2 = 4,
    parameter NUM_FIR = 160,
    parameter NUM_FIR_LOG2 = 8,
    parameter DECIM = 147,
    parameter BANK_WIDTH = NUM_FIR_LOG2+HALFDEPTH_LOG2,

    parameter TIMESLICE = 64, // Not sure if this is OK.
    parameter TIMESLICE_LOG2 = 6
)(
    input wire clk,
    input wire rst,
    input wire [(NUM_CH-1):0] rst_ch,

    // to firbank
    output wire [(BANK_WIDTH-1):0] bank_addr_o,
    input wire [23:0] bank_data_i,

    // data input wire
    input wire [(NUM_CH-1):0] ack_i,
    input wire [(24*NUM_CH-1):0] data_i,
    output wire [(NUM_CH-1):0] pop_o,

    // data output wire
    input wire [(NUM_CH-1):0] pop_i,
    output wire [23:0] data_o,
    output wire [(NUM_CH-1):0] ack_o);

wire [(NUM_CH-1):0] pop;
wire [(HALFDEPTH_LOG2+1-1):0] rb_offset;
wire [(NUM_CH*24-1):0] rb_data;

genvar ig;
generate
for (ig = 0; ig < NUM_CH; ig = ig + 1) begin:rbunit
    ringbuf #(
        .LEN(HALFDEPTH*4), // should work w/ *2, but buffer a little longer to address input wire jitter
        .LEN_LOG2(HALFDEPTH_LOG2+2)
    ) rb(
        .clk(clk), .rst(rst | rst_ch[ig]),
        .data_i(data_i[(24*ig)+:24]), .we_i(ack_i[ig]),
        .pop_i(pop[ig]), .offset_i({1'b0, rb_offset[HALFDEPTH_LOG2:0]}), .data_o(rb_data[(24*ig) +: 24]));
end
endgenerate

resampler_core #(
    .NUM_CH(NUM_CH), .NUM_CH_LOG2(NUM_CH_LOG2),
    .HALFDEPTH(HALFDEPTH), .HALFDEPTH_LOG2(HALFDEPTH_LOG2),
    .NUM_FIR(NUM_FIR), .NUM_FIR_LOG2(NUM_FIR_LOG2), .DECIM(DECIM),
    .TIMESLICE(TIMESLICE), .TIMESLICE_LOG2(TIMESLICE_LOG2)
) core(
    .clk(clk), .rst(rst),
    .bank_addr_o(bank_addr_o), .bank_data_i(bank_data_i),
    .pop_o(pop), .offset_o(rb_offset), .data_i(rb_data),
    .pop_i(pop_i), .data_o(data_o), .ack_o(ack_o)
    );

assign pop_o = pop;

endmodule
