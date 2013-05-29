
module spdif_dai #(
parameter CLK_PER_BIT = 4
)(
    input clk,
    input rst,

    input signal,

    output locked
);

parameter PHASE_SAMELVL_END = CLK_PER_BIT/2*3 - 1;
parameter PHASE_SYNC_CODE_END = CLK_PER_BIT/2*8 - 1;
parameter PHASE_SUBFRAME_END = CLK_PER_BIT*64 - 1;

parameter ST_FIND_START = 0;
parameter ST_FIND_B = 1;
parameter ST_LOCKED = 2;
reg [2:0] state;

reg [10:0] phase_counter;

reg sync_start_lvl_ff;
reg [10:0] sync_code_ff;

reg [(CLK_PER_BIT-2):0] recent_lvl_hist_ff;
wire [(CLK_PER_BIT-1):0] recent_lvl = {recent_lvl_hist_ff, signal};
wire prev_lvl = recent_lvl_hist_ff[1];

// a bit in syncblk. only valid when syncblk_bit_valid
wire syncblk_bit = recent_lvl == 2'b11; // FIXME: support variable CLK_PER_BIT
wire syncblk_bit_valid = phase_counter & (CLK_PER_BIT/2-1) == (CLK_PER_BIT/2-1);
reg [4:0] prev_syncblk_ff;
wire curr_syncblk = {prev_syncblk_ff, syncblk_bit};

parameter SYNC_BLK_B1 = 6'b010111;
parameter SYNC_BLK_B2 = 6'b101000;

always @(posedge clk) begin
    if(rst) begin
        phase_counter <= 0;
        state <= ST_FIND_START;
    end else begin
        recent_lvl_hist_ff <= recent_lvl[(CLK_PER_BIT-2):0];

        if(phase_counter == PHASE_SUBFRAME_END)
            phase_counter <= 0;
        else
            phase_counter <= phase_counter + 1;
                    
        case(state)
            ST_FIND_START: begin
                if(signal == prev_lvl) begin
                    if(phase_counter == PHASE_SAMELVL_END) begin
                        sync_start_lvl_ff <= signal;

                        state <= ST_FIND_B;
                    end
                end else
                    phase_counter <= 0;
            end
            ST_FIND_B: begin
                if(phase_counter == 0)
                    sync_start_lvl_ff <= signal;
                    prev_syncblk_ff <= {4'b0, signal};
                else if (phase_counter <= PHASE_SAMELVL_END) begin
                    // verify that still at same lvl
                    if(signal != sync_start_lvl_ff) begin
                        phase_counter <= 0;
                        state <= ST_FIND_START;
                    end
                end else if (phase_counter <= PHASE_SYNC_CODE_END) begin
                    if(syncblk_bit_valid) begin
                        prev_syncblk_ff <= curr_syncblk[4:0];

                        if(phase_counter == PHASE_SYNC_CODE_END) begin
                            if(curr_syncblk == SYNC_BLK_B1 || curr_syncblk == SYNC_BLK_B2) begin
                                state <= ST_LOCKED;
                            end
                        end
                    end
                end
            end
            ST_LOCKED: begin

            end
            default: state <= ST_FIND_START;
        endcase
    end
end

endmodule
