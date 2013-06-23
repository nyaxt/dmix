`define DEBUG

module resampler_1ch
#(
    parameter FIRDEPTH = 32,
    parameter FIRDEPTH_LOG2 = 5,
    parameter NUM_FIR = 2,
    parameter NUM_FIR_LOG2 = 1,
    parameter DECIM = 1,
    parameter MULT_LATENCY = 4,
    parameter BANK_WIDTH = FIRDEPTH_LOG2+NUM_FIR_LOG2
)(
    input clk,
    input rst,

    // to firbank
    output [(BANK_WIDTH-1):0] bank_addr_o,
    input [15:0] bank_data_i,

    // to multiplier
    input mpready_i,
    output signed [23:0] mpcand_o,
    output signed [15:0] mplier_o,
    input signed [23:0] mprod_i,

    // to ringbuf
    output pop_o,
    output [(FIRDEPTH_LOG2-1):0] offset_o,
    input [23:0] data_i,

    // data output
    input pop_i,
    output [23:0] data_o,
    output ack_o);

parameter ST_IDLE = 0;
parameter ST_RESULT = 1;
parameter ST_CALC = 2;
parameter ST_NEXT_FIR = 3;
reg [3:0] state;

parameter PIPELINEDEPTH = 1 + MULT_LATENCY + 1;

reg [(NUM_FIR_LOG2-1):0] firidx_ff;
reg [(NUM_FIR_LOG2-1):0] pop_counter;

reg [(FIRDEPTH_LOG2+1-1):0] depthidx_ff; // +1 is because depthidx_ff max is FIRDEPTH + PIPELINEDEPTH
assign offset_o = depthidx_ff[(FIRDEPTH_LOG2-1):0];
assign bank_addr_o = {firidx_ff, depthidx_ff[(FIRDEPTH_LOG2-1):0]};

reg signed [23:0] sample_ff;
wire mpcand_o = sample_ff : 0;
reg signed [15:0] coeff_ff;
wire mplier_o = coeff_ff : 0;

reg signed [23:0] result_ff;
reg pop_ff;
assign pop_o = pop_ff;

always @(posedge clk) begin
    if(rst) begin
        state <= ST_IDLE;

        sample_ff <= 0;
        coeff_ff <= 0;
        result_ff <= 0;

        depthidx_ff <= 0;
        firidx_ff <= 0;
        pop_counter <= 0;
    end else begin
        pop_ff <= 0;
        sample_ff <= 0;
        coeff_ff <= 0;

        case(state)
        ST_IDLE: begin
            if(pop_i) begin
                state <= ST_RESULT;
            end
        end
        ST_RESULT: begin
            depthidx_ff <= 0;
            result_ff <= 0;

            if(mpready)
                state <= ST_CALC;
        end
        ST_CALC: begin
            // PIPELINE STAGE 1: load sample / filter coeff
            sample_ff <= data_i;
            coeff_ff <= bank_data_i;
            
            // PIPELINE STAGE 2-5: mul
`ifdef DEBUG
            if(!mpready_i)
                $display("mp must be always ready when ST_CALC");
`endif

            // PIPELINE STAGE 6: add
            if(depthidx_ff >= PIPELINEDEPTH-1)
                result_ff <= result_ff + mpprod;

            if(depthidx_ff == FIRDEPTH+PIPELINEDEPTH)
                state <= ST_NEXT_FIR;
            else
                depthidx_ff <= depthidx_ff + 1;
        end
        ST_NEXT_FIR: begin
            if(firidx_ff == NUM_FIR-1)
                firidx_ff <= 0;
            else
                firidx_ff <= firidx_ff + 1;

            if(pop_counter >= NUM_FIR - 1) begin
                pop_counter <= pop_counter + 1 - NUM_FIR;
                pop_ff <= 1;
            end else
                pop_counter <= pop_counter + 1;

            state <= ST_IDLE;
        end
        endcase
    end
end

assign ack_o = (state == ST_RESULT);
assign data_o = result_ff;

endmodule

module upsample2x_1ch(
    input clk,
    input rst,

    // external multiplier module
    input mpready_i,
    output signed [23:0] mpcand_o,
    output signed [15:0] mplier_o,
    input signed [23:0] mprod_i,

    // data input
    output pop_o,
    input [23:0] data_i,
    input ack_i,

    // data output
    input pop_i,
    output [23:0] data_o,
    output ack_o);

wire [5:0] fb_addr;
wire [15:0] fb_data;

rom_firbank_half fb(.addr(fb_addr), .data(fb_data));

wire rb_pop;
wire [4:0] rb_offset;
wire [23:0] rb_data;

ringbuf #(
    .LEN(64), // should work w/ 32, but buffer a little to address input jitter
    .LEN_LOG2(6)
) rb(
    .clk(clk), .rst(rst),
    .data_i(data_i), .we_i(ack_i),
    .pop_i(rb_pop), .offset_i({1'b0, rb_offset}), .data_o(rb_data));
assign pop_o = rb_pop;

resampler_1ch r(
    .clk(clk), .rst(rst),
    .bank_addr_o(fb_addr), .bank_data_i(fb_data),
    .mpready_i(mpready_i), .mpcand_o(mpcand_o), .mplier_o(mplier_o), .mprod_i(mprod_i),
    .pop_o(rb_pop), .offset_o(rb_offset), .data_i(rb_data),
    .pop_i(pop_i), .data_o(data_o), .ack_o(ack_o));

endmodule
