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

    // shared resource ready
    input shres_ready_i,

    // to firbank
    output [(BANK_WIDTH-1):0] bank_addr_o,
    input [15:0] bank_data_i,

    // to multiplier
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
// below is to be 0 when !shres_ready_i. depthidx_ff is guaranteed to be 0 when !shres_ready_i.
assign bank_addr_o = {(shres_ready_i ? firidx_ff : 0), depthidx_ff[(FIRDEPTH_LOG2-1):0]};

reg signed [23:0] sample_ff;
wire mpcand_o = sample_ff;
reg signed [15:0] coeff_ff;
wire mplier_o = coeff_ff;

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
        depthidx_ff <= 0;

        case(state)
        ST_IDLE: begin
            if(pop_i) begin
                state <= ST_RESULT;
            end
        end
        ST_RESULT: begin
            result_ff <= 0;

            if(shres_ready_i)
                state <= ST_CALC;
        end
        ST_CALC: begin
            // PIPELINE STAGE 1: load sample / filter coeff
            sample_ff <= data_i;
            coeff_ff <= bank_data_i;
            
            // PIPELINE STAGE 2-5: mul
`ifdef DEBUG
            if(!shres_ready_i)
                $display("mp must be always ready when ST_CALC");
`endif

            // PIPELINE STAGE 6: add
            if(depthidx_ff >= PIPELINEDEPTH-1)
                result_ff <= result_ff + mprod_i;

            if(depthidx_ff == FIRDEPTH+PIPELINEDEPTH-1)
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
assign data_o = ack_o ? result_ff : 0;

endmodule

module upsample2x(
    input clk,
    input rst,

    // external multiplier module
    input mpready_i,
    output signed [23:0] mpcand_o,
    output signed [15:0] mplier_o,
    input signed [23:0] mprod_i,

    // data input
    output [1:0] pop_o,
    input [23:0] data_i,
    input [1:0] ack_i,

    // data output
    input [1:0] pop_i,
    output [23:0] data_o,
    output [1:0] ack_o);

reg pop_lr;
always @(posedge clk) begin
    if(pop_i[0])
        pop_lr <= 0;
    else if(pop_i[1])
        pop_lr <= 1;
end
wire shres_ready [1:0];
assign shres_ready[0] = pop_lr == 0 && mpready_i;
assign shres_ready[1] = pop_lr == 1 && mpready_i;

wire [5:0] fb_addr_lr [1:0];
wire [5:0] fb_addr = fb_addr_lr[0] | fb_addr_lr[1];
wire [15:0] fb_data;

rom_firbank_half fb(.addr(fb_addr), .data(fb_data));

wire signed [23:0] mpcand_lr_o [1:0];
wire signed [15:0] mplier_lr_o [1:0];
// shres_ready ? will take additional resource as this will be a huge OR
// spanning multiple upsample2x
assign mpcand_o = mpcand_lr_o[0] | mpcand_lr_o[1];
assign mplier_o = mplier_lr_o[0] | mplier_lr_o[1];

wire [1:0] rb_pop;
wire [4:0] rb_offset [1:0];
wire [23:0] rb_data [1:0];

assign pop_o = rb_pop;

wire signed [23:0] data_lr_o [1:0];
assign data_o = data_lr_o[0] | data_lr_o[1];

genvar i;
generate
for(i = 0; i < 2; i = i + 1) begin:g
    ringbuf #(
        .LEN(64), // should work w/ 32, but buffer a little to address input jitter
        .LEN_LOG2(6)
    ) rb(
        .clk(clk), .rst(rst),
        .data_i(data_i), .we_i(ack_i[i]),
        .pop_i(rb_pop[i]), .offset_i({1'b0, rb_offset[i]}), .data_o(rb_data[i]));

    resampler_1ch r(
        .clk(clk), .rst(rst),
        .shres_ready_i(shres_ready[i]), 
        .bank_addr_o(fb_addr_lr[i]), .bank_data_i(fb_data),
        .mpcand_o(mpcand_lr_o[i]), .mplier_o(mplier_lr_o[i]), .mprod_i(mprod_i),
        .pop_o(rb_pop[i]), .offset_o(rb_offset[i]), .data_i(rb_data[i]),
        .pop_i(pop_i[i]), .data_o(data_lr_o[i]), .ack_o(ack_o[i]));
end
endgenerate

endmodule
