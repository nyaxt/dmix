// FIXME: stop using curr_ and load at next clk instead. will cause more
// latency fore sure

module spdif_dai #(
parameter CLK_PER_BIT = 4,
parameter SUBFRAME_WIDTH = 28,
)(
    input clk,
    input rst,

    input signal,

    output [23:0] data_o,
    output we_o,
    output locked
);

parameter PHASE_SAMELVL_END = CLK_PER_BIT/2*3 - 1;
parameter PHASE_SYNC_CODE_END = CLK_PER_BIT/2*8 - 1;
parameter PHASE_AUDIODATA_END = CLK_PER_BIT*(4+24) - 1;
parameter PHASE_SUBFRAME_END = CLK_PER_BIT*32 - 1;

parameter ST_FIND_START = 0;
parameter ST_FIND_B = 1;
parameter ST_LOCKED = 2;
reg [2:0] state;
assign locked = (state == ST_LOCKED);

reg [10:0] phase_counter;

reg sync_start_lvl_ff;
reg [10:0] sync_code_ff;

reg [(CLK_PER_BIT-2):0] recent_lvl_hist_ff;
wire [(CLK_PER_BIT-1):0] recent_lvl = {recent_lvl_hist_ff, signal};
wire prev_lvl = recent_lvl_hist_ff[1];

// a bit in syncblk. only valid when syncblk_bit_valid
wire syncblk_bit = recent_lvl[1:0] == 2'b11; // FIXME: support variable CLK_PER_BIT
wire syncblk_bit_valid = phase_counter & (CLK_PER_BIT/2-1) == (CLK_PER_BIT/2-1);
reg [4:0] prev_syncblk_ff;
wire curr_syncblk = {prev_syncblk_ff, syncblk_bit};

// FIXME: support variable CLK_PER_BIT
function bmcdecode(
    input [3:0] lvl,
    input last);
reg [3:0] plvl;

begin
    plvl = {(last ? ~lvl[3:2] : lvl[3:2]), lvl[1:0]}
    case(plvl)
        // obvious
        4'b0000, 4'b0001, 4'b0010, 4'b0100, 4'b1000: bmcdecode = 0;
        4'b1111, 4'b1110, 4'b1101, 4'b1011, 4'b0111: bmcdecode = 1;
        // hmm..., use first, then middle?
        4'b1100, 4'b0110, 4'b1010: bmcdecode = 1;
        4'b0011, 4'b1001, 4'b0101: bmcdecode = 0;
    endcase
end
endfunction

wire bmcdecode_bit = bmcdecode(recent_lvl);
wire bmcdecode_bit_valid = phase_counter & (CLK_PER_BIT-1) == (CLK_PER_BIT-1);

reg [(SUBFRAME_WIDTH-1):0] data_ff;
assign data_o = data_ff[23:0];
assign we_o = (phase_counter == PHASE_AUDIODATA_END+1) & locked;

parameter SYNC_BLK_B1 = 6'b010111;
parameter SYNC_BLK_B2 = 6'b101000;

always @(posedge clk) begin
    if(rst) begin
        phase_counter <= 0;
        data_ff <= 0;

        state <= ST_FIND_START;
    end else begin
        recent_lvl_hist_ff <= recent_lvl[(CLK_PER_BIT-2):0];

        if(phase_counter == PHASE_SUBFRAME_END)
            phase_counter <= 0;
        else
            phase_counter <= phase_counter + 1;

        // audio / control data
        if(bmcdecode_bit_valid)
            data_ff <= {data_ff[(SUBFRAME_WIDTH-2):0], bmcdecode_bit};

        if(phase_counter == 0)
            prev_syncblk_ff <= {4'b0, signal};
        else if(phase_counter <= PHASE_SAMELVL_END) begin
            // verify that still at same lvl
            if(signal != prev_syncblk_ff[0]) begin
                prev_syncblk_ff <= {4'b0, signal};
                phase_counter <= 1;
                state <= ST_FIND_START;
            end
        end else if (phase_counter <= PHASE_SYNC_CODE_END) begin
            if(syncblk_bit_valid)
                prev_syncblk_ff <= curr_syncblk[4:0];

            // FIXME: check B/M/W?
        end else begin
            // handle control data
            if(phase == PHASE_FETCH_UDATA)
                u_data_ff <= {u_data_ff[191:0], data[0]};
            if(phase == PHASE_FETCH_CDATA)
                c_data_ff <= {c_data_ff[191:0], data[0]};
        end
                    
        case(state)
            ST_FIND_START: begin
                if(signal == prev_syncblk_ff[0] && phase_counter == PHASE_SAMELVL_END)
                    state <= ST_FIND_B;
            end
            ST_FIND_B: begin
                if(phase_counter == PHASE_SYNC_CODE_END) begin
                    if(curr_syncblk == SYNC_BLK_B1 || curr_syncblk == SYNC_BLK_B2) begin
                        state <= ST_LOCKED;
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
