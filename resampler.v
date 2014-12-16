module resampler_core
#(
    parameter NUM_CH = 8,
    parameter NUM_CH_LOG2 = 3,

    parameter HALFDEPTH = 16,
    parameter HALFDEPTH_LOG2 = 4,
    parameter NUM_FIR = 160,
    parameter NUM_FIR_LOG2 = 8,
    parameter DECIM = 147,
    parameter MULT_LATENCY = 5,
    parameter BANK_WIDTH = NUM_FIR_LOG2+HALFDEPTH_LOG2,

    parameter TIMESLICE = 64, // Not sure if this is OK.
    parameter TIMESLICE_LOG2 = 6
)(
    input clk,
    input rst,

    // to firbank
    output [(BANK_WIDTH-1):0] bank_addr_o,
    input [23:0] bank_data_i,

    // to ringbuf array
    output [(NUM_CH-1):0] pop_o,
    output [((HALFDEPTH_LOG2+1)*NUM_CH-1):0] offset_o,
    input [(24*NUM_CH-1):0] data_i,

    // data output
    input [(NUM_CH-1):0] pop_i,
    output [23:0] data_o,
    output [(NUM_CH-1):0] ack_o);

// Latch pop_i request
reg [(NUM_CH-1):0] pop_i_latch;
wire [(NUM_CH-1):0] ack_pop_i;
always @(posedge clk) begin
    if (rst) begin
        pop_i_latch <= 0;
    end else begin
        pop_i_latch <= pop_i | (~ack_pop_i & pop_i_latch);
    end
end

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
reg processing_enabled_ff;
reg [7:0] state_ff; // Note: bit width will be optimized anyway.
parameter ST_READY = 0; // 1 clk
parameter ST_BEGIN_CYCLE = 1;  // 1 clk
parameter ST_MULADD_RWING = 2; // MULT_LATENCY + HALFDEPTH clk
parameter ST_PREP_LWING = 3; // 1 clk
parameter ST_MULADD_LWING = 4; // MULT_LATENCY + HALFDEPTH clk
parameter ST_END_CYCLE = 5; // 1 clk
parameter ST_IDLE = 6;

reg [(NUM_CH-1):0] ack_pop_ff;
assign ack_pop_i = ack_pop_ff;

reg [HALFDEPTH_LOG2:0] muladd_wing_cycle_counter;

always @(posedge clk) begin
    if (rst_processing_ff) begin
        ack_pop_ff <= 1 << processing_ch_ff;
        processing_enabled_ff <= pop_i_latch[processing_ch_ff];
        state_ff <= ST_READY;
    end else begin
        muladd_wing_cycle_counter <= muladd_wing_cycle_counter + 1;

        ack_pop_ff <= 0;
        case (state_ff)
            ST_READY: begin
                if (processing_enabled_ff)
                    state_ff <= ST_BEGIN_CYCLE;
            end
            ST_BEGIN_CYCLE: begin
                state_ff <= ST_MULADD_RWING;
                muladd_wing_cycle_counter <= 0;
            end
            ST_MULADD_RWING: begin
                if (muladd_wing_cycle_counter == 1 + MULT_LATENCY + HALFDEPTH)
                    state_ff <= ST_PREP_LWING;
            end
            ST_PREP_LWING: begin
                state_ff <= ST_MULADD_LWING;
                muladd_wing_cycle_counter <= 0;
            end
            ST_MULADD_LWING: begin
                if (muladd_wing_cycle_counter == 1 + MULT_LATENCY + HALFDEPTH)
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
    firidx_lwing_currch_ff <= NUM_FIR-1 - firidx_rwing_currch_ff;
    pop_o_ff <= 0;

    if (rst) begin
        for (i = 0; i < NUM_CH; i = i + 1) begin
            firidx_mem[i] <= 0;
        end
        
        firidx_rwing_currch_ff <= 0;
    end else begin
        case (state_ff)
            ST_BEGIN_CYCLE:
                firidx_rwing_currch_ff <= firidx_mem[processing_ch_ff];
            ST_END_CYCLE: begin
                if (firidx_rwing_currch_ff > NUM_FIR - DECIM) begin
                    firidx_mem[processing_ch_ff] <= firidx_rwing_currch_ff + DECIM - NUM_FIR;
                    pop_o_ff[processing_ch_ff] <= 1;
                end else begin
                    firidx_mem[processing_ch_ff] <= firidx_rwing_currch_ff + DECIM;
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
    ST_BEGIN_CYCLE:
        depthidx_ff <= HALFDEPTH-1;
    ST_MULADD_RWING:
        depthidx_ff <= depthidx_ff - 1;
    ST_MULADD_LWING:
        depthidx_ff <= depthidx_ff + 1;
endcase
end

// Supply mpcand
wire [23:0] mpcand_o = data_i;
assign offset_o = {~mplier_lwing_active, muladd_wing_cycle_counter[(HALFDEPTH_LOG2-1):0]};

// Multiplier
wire [23:0] mprod_i;
mpemu mpemu(.clk(clk), .mpcand_i(mpcand_o), .mplier_i(mplier_o), .mprod_o(mprod_i));
wire product_valid = muladd_wing_cycle_counter > MULT_LATENCY+1 && (state_ff == ST_MULADD_LWING || state_ff == ST_MULADD_RWING);

// Adder
reg [23:0] sum_ff;
always @(posedge clk) begin
    if (state_ff == ST_BEGIN_CYCLE)
        sum_ff <= 0;
    else if (product_valid)
        sum_ff <= $signed(sum_ff) + $signed(mprod_i);
end

// Result
assign data_o = sum_ff;
assign ack_o = (state_ff == ST_END_CYCLE ? 1 : 0) << processing_ch_ff;

endmodule
