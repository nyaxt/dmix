`default_nettype none
module ddr3_fb(
    input wire clk, // clk491520
    input wire rst,

    // MIG interface
    output wire mig_cmd_clk,
    output wire mig_cmd_en,
    output wire [2:0] mig_cmd_instr,
    output wire [5:0] mig_cmd_bl,
    output wire [29:0] mig_cmd_byte_addr,
    input wire mig_cmd_empty,
    input wire mig_cmd_full,
    output wire mig_wr_clk,
    output wire mig_wr_en,
    output wire [3:0] mig_wr_mask,
    output wire [31:0] mig_wr_data,
    input wire mig_wr_full,
    input wire mig_wr_empty,
    input wire [6:0] mig_wr_count,
    input wire mig_wr_underrun,
    input wire mig_wr_error,
    output wire mig_rd_clk,
    output wire mig_rd_en,
    input wire [31:0] mig_rd_data,
    input wire mig_rd_full,
    input wire mig_rd_empty,
    input wire [6:0] mig_rd_count,
    input wire mig_rd_overflow,
    input wire mig_rd_error,

    // To LCDC
    input wire [8:0] x_i,
    input wire [6:0] y_i,
    input wire in_hsync_i,
    input wire in_vsync_i,
    input wire pop_i,
    output wire [5:0] r_o,
    output wire [5:0] g_o,
    output wire [5:0] b_o,
    output wire ack_o);

reg [3:0] state_ff;
localparam ST_INIT = 1;
localparam ST_EMIT_CMD = 2;
localparam ST_WAIT_DATA = 4;

wire [5:0] mig_rd_data_r;
wire [5:0] mig_rd_data_g;
wire [5:0] mig_rd_data_b;
assign mig_rd_data_r = mig_rd_data[24:18];
assign mig_rd_data_g = mig_rd_data[15:10];
assign mig_rd_data_b = mig_rd_data[7:2];

reg [8:0] prefetch_x_ff;
reg [6:0] prefetch_y_ff;
reg [8:0] prefetch_xzero_done_ff;
reg lcdout_side_ff;

reg [5:0] r_cache_mem [15:0];
reg [5:0] g_cache_mem [15:0];
reg [5:0] b_cache_mem [15:0];

wire [3:0] cache_mem_wr_addr;
assign cache_mem_wr_addr = {~lcdout_side_ff, prefetch_x_ff[2:0]};

always @(posedge clk) begin
    if (rst) begin
        state_ff <= ST_INIT;
        prefetch_x_ff <= 'b0;
        prefetch_y_ff <= 'b0;
        prefetch_xzero_done_ff <= 1'b0;
        lcdout_side_ff <= 1'b0;
    end else begin
        case (state_ff)
            ST_INIT: begin
                if (pop_i == 1'b1 && x_i[2:0] == 3'h0) begin
                    if (in_hsync_i == 1'b1) begin
                        prefetch_x_ff <= 9'h000;
                        prefetch_xzero_done_ff <= 1'b1;
                    end else begin
                        prefetch_x_ff <= x_i + 9'h008;
                        prefetch_xzero_done_ff <= 1'b0;
                    end

                    prefetch_y_ff <= y_i;
                    
                    if (in_vsync_i == 1'b0 && prefetch_xzero_done_ff == 1'b0) begin
                        state_ff <= ST_EMIT_CMD;
                    end
                end
            end
            ST_EMIT_CMD: begin
                state_ff <= ST_WAIT_DATA;
            end
            ST_WAIT_DATA: begin
                if (mig_rd_count == 6'h00) begin // FIXME: mig_rd_empty?
                    state_ff <= ST_WAIT_DATA;
                end else begin
                    r_cache_mem[cache_mem_wr_addr] <= mig_rd_data_r;
                    g_cache_mem[cache_mem_wr_addr] <= mig_rd_data_g;
                    b_cache_mem[cache_mem_wr_addr] <= mig_rd_data_b;

                    prefetch_x_ff[2:0] <= prefetch_x_ff[2:0] + 1;
                    if (prefetch_x_ff[2:0] != 3'h7) begin
                        state_ff <= ST_WAIT_DATA;
                    end else begin
                        lcdout_side_ff <= ~lcdout_side_ff;
                        state_ff <= ST_INIT;
                    end
                end
            end
        endcase
    end
end

assign mig_cmd_clk = clk;
assign mig_cmd_en = (state_ff == ST_EMIT_CMD) ? 1'b1 : 1'b0;
assign mig_cmd_instr = 3'b001;
assign mig_cmd_bl = 6'h08;
assign mig_cmd_byte_addr[29:0] = {12'h000, prefetch_y_ff, prefetch_x_ff, 2'b00};
// FIXME: wait until mig_cmd_empty or !mig_cmd_full?

assign mig_wr_clk = clk;
assign mig_wr_en = 1'b0;
assign mig_wr_mask = 4'b0000;
assign mig_wr_data = 32'h0000;
// mig_wr_full, mig_wr_empty, mig_wr_count, mig_wr_underrun, mig_wr_error

assign mig_rd_clk = clk;
assign mig_rd_en = (state_ff == ST_WAIT_DATA) ? 1'b1 : 1'b0;
// mig_rd_full, mig_rd_empty, mig_rd_overflow, mig_rd_error

reg [5:0] r_ff;
reg [5:0] g_ff;
reg [5:0] b_ff;
reg ack_ff;

wire [3:0] cache_mem_rd_addr;
assign cache_mem_rd_addr = {lcdout_side_ff, x_i[2:0]};

always @(posedge clk) begin
    if (rst) begin
        r_ff <= 6'h00;
        g_ff <= 6'h00;
        b_ff <= 6'h00;
        ack_ff <= 1'b1;
    end else begin
        if (pop_i) begin
            r_ff <= r_cache_mem[cache_mem_rd_addr];
            g_ff <= g_cache_mem[cache_mem_rd_addr];
            b_ff <= b_cache_mem[cache_mem_rd_addr];
            ack_ff <= 1'b1;
        end else begin
            ack_ff <= 1'b0;
        end
    end
end
assign r_o = r_ff;
assign g_o = g_ff;
assign b_o = b_ff;
assign ack_o = ack_ff;

endmodule
`default_nettype wire
