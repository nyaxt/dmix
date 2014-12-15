module resampler_core
#(
    parameter NUM_CH = 8,
    parameter NUM_CH_LOG2 = 3,

    parameter FIRDEPTH = 32,
    parameter FIRDEPTH_LOG2 = 5,
    parameter NUM_FIR = 160,
    parameter NUM_FIR_LOG2 = 8,
    parameter DECIM = 147,
    parameter MULT_LATENCY = 4,
    parameter BANK_WIDTH = FIRDEPTH_LOG2+NUM_FIR_LOG2,

    parameter TIMESLICE = 48, // Not sure if this is OK.
    parameter TIMESLICE_LOG2 = 6
)(
    input clk,
    input rst,

    // to firbank
    output [(BANK_WIDTH-1):0] bank_addr_o,
    input [23:0] bank_data_i,

    // to ringbuf array
    output [(NUM_CH-1):0] pop_o,
    output [(FIRDEPTH_LOG2*NUM_CH-1):0] offset_o,
    input [(24*NUM_CH-1):0] data_i,

    // data output
    input [(NUM_CH-1):0] pop_i,
    output [(24*NUM_CH-1):0] data_o,
    output ack_o);

// Latch pop_i request
reg [(NUM_CH-1):0] pop_i_latch;
wire [(NUM_CH-1);0] ack_pop_i;
always @(posedge clk) begin
    if (rst) begin
        pop_i_latch <= 0;
    end else begin
        pop_i_latch <= pop_i | (~ack_pop_i & pop_i_latch);
    end
end

// Fixed timeslice based scheduling and decide ch to process
reg [(NUM_CH_LOG2-1):0] processing_ch_ff;

reg [(TIMESLICE_LOG2-1):0] timeslice_counter;
wire timeslice_deadline = (timeslice_counter == TIMESLICE-1)

always @(posedge clk) begin
    if (rst) begin
        timeslice_counter <= 0;
        processing_ch_ff <= 0;
        rst_processing <= 1;
    end else begin
        timeslice_counter <= timeslice_counter + 1;

        if (timeslice_deadline) begin
            processing_ch_ff <= processing_ch_ff + 1;
            rst_processing <= 1;
        end
    end
end

// Sequence management
reg processing_enabled_ff;
reg [7:0] state_ff; // Note: bit width will be optimized anyway.
parameter ST_IDLE = 0; // 1 clk
parameter ST_BEGIN_CYCLE = 1;  // 1 clk
parameter ST_MULADD_RWING = 2; // MULT_LATENCY + NUM_FIR clk
parameter ST_PREP_LWING = 3; // 1 clk
parameter ST_MULADD_LWING = 4; // MULT_LATENCY + NUM_FIR clk
parameter ST_END_CYCLE = 5; // 1 clk

reg [(NUM_CH-1);0] ack_pop_ff;
assign ack_pop_i = ack_pop_ff;

always @(posedge clk) begin
    if (rst_processing) begin
        ack_pop_ff <= 1 << processing_ch_ff;
        processing_enabled_ff <= pop_i_latch[processing_ch_ff];
        state_ff <= ST_IDLE
    end else begin
        ack_pop_ff <= 0;
        case (state) begin
            ST_IDLE: begin
                if (processing_enabled_ff)
                    state <= ST_BEGIN_CYCLE;
            end
            ST_BEGIN_CYCLE:
                state <= ST_MULADD_LWING;
        endcase
    end
end

// Compute polyphase FIR filter index
// OUTPUT:
reg [(NUM_FIR_LOG2-1):0] firidx_rwing_currch_ff;
reg [(NUM_FIR_LOG2-1):0] firidx_lwing_currch_ff;

reg [(NUM_FIR_LOG2-1):0] firidx_mem [(NUM_CH-1):0];

always @(posedge clk) begin
    integer i;

    firidx_lwing_currch_ff <= NUM_FIR-1 - firidx_rwing_currch;
    if (rst) begin
        for (i = 0; i < NUM_CH; ++ i)
            firidx_lidx_mem[i] <= 0;
        
        firidx_rwing_currch_ff <= 0;
    end else begin
        case (state)
            ST_BEGIN_CYCLE:
                firidx_rwing_currch_ff <= firidx_lidx_ff[processing_ch_ff];
            ST_INC_FIRIDX: begin
                if (firidx_rwing_currch_ff > NUM_FIR - DECIM) begin
                    firidx_lidx_ff[processing_ch_ff] <= firidx_rwing_currch + DECIM - NUM_FIR;
                end else begin
                    firidx_lidx_ff[processing_ch_ff] <= firidx_rwing_currch + DECIM;
                end
            end
        endcase
    end 
end

// Supply mplier
// OUTPUT:
wire [23:0] mplier_o = bank_data_i;

wire mplier_lwing_active = state_ff == ST_MULADD_LWING;
wire firidx = mplier_lwing_active ? firidx_lwing_currch_ff : firidx_rwing_currch_ff;
reg [(FIRDEPTH_LOG2-1):0] depthidx_ff;

assign bank_addr_o = {firidx, depthidx_ff};
always @(posedge clk) begin
    case (state)
    ST_BEGIN_CYCLE:
        depthidx_ff <= NUM_FIR-1;
    ST_MULADD_RWING:
        depthidx_ff <= depthidx_ff - 1;
    ST_MULADD_LWING:
        depthidx_ff <= depthidx_ff + 1;
    end
end

endmodule
