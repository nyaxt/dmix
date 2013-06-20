module resampler_1ch(
    input clk,
    input rst,

    // to ringbuf
    output pop_o,
    output [3:0] offset_o,
    input [23:0] data_i,

    // to mixer
    input pop_i,
    output [23:0] data_o,
    output ack_o);

parameter FIRDEPTH = 16;
parameter FIRDEPTH_LOG2 = 4;
parameter NUM_FIR = 123;
parameter NUM_FIR_LOG2 = 7;
parameter DECIM = 31;
reg [15:0] firbank [(NUM_FIR*FIRDEPTH):0];

parameter ST_IDLE = 0;
parameter ST_RESULT = 1;
parameter ST_CALC = 2;
parameter ST_NEXT_FIR = 3;
reg [3:0] state;

parameter PIPELINEDEPTH = 2;

reg [(NUM_FIR_LOG2-1):0] firidx_ff;
reg [(NUM_FIR_LOG2-1):0] pop_counter;
wire [(NUM_FIR_LOG2+1-1):0] pop_counter_next = pop_counter + DECIM;

reg [(FIRDEPTH_LOG2+1-1):0] depthidx_ff;
assign offset_o = depthidx_ff;

reg [23:0] sample_ff;
reg [15:0] coeff_ff;

reg [39:0] seki_ff;

reg [39:0] result_ff;
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

        case(state)
        ST_IDLE: begin
            if(pop_i) begin
                state <= ST_RESULT;
            end
        end
        ST_RESULT: begin
            state <= ST_CALC;

            sample_ff <= 0;
            coeff_ff <= 0;

            depthidx_ff <= 0;

            result_ff <= 0;
        end
        ST_CALC: begin
            // PIPELINE STAGE 1: load sample / filter coeff
            sample_ff <= data_i;
            coeff_ff <= firbank[firidx_ff*FIRDEPTH + depthidx_ff];
            
            // PIPELINE STAGE 2: mul
            // FIXME: coeff signed!
            seki_ff <= sample_ff * coeff_ff;

            // PIPELINE STAGE 3: add
            // FIXME: result_ff signed!
            result_ff <= result_ff + seki_ff;

            if(depthidx_ff == FIRDEPTH+PIPELINEDEPTH)
                state <= ST_NEXT_FIR;
        end
        ST_NEXT_FIR: begin
            if(firidx_ff == NUM_FIR-1)
                firidx_ff <= 0;
            else
                firidx_ff <= firidx_ff + 1;

            if(pop_counter_next > NUM_FIR) begin
                pop_counter <= pop_counter_next - NUM_FIR;
                pop_ff <= 1;
            end else
                pop_counter <= pop_counter_next;

            state <= ST_IDLE;
        end
        endcase
    end
end

assign ack_o = (state == ST_RESULT);
assign data_o = result_ff;

endmodule
