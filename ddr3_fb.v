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
    input wire pop_i,
    output wire [5:0] r_o,
    output wire [5:0] g_o,
    output wire [5:0] b_o,
    output wire ack_o);

reg [3:0] state_ff;
localparam ST_INIT = 1;
localparam ST_EMIT_CMD = 2;
localparam ST_WAIT_DATA = 4;
localparam ST_PUSH_DATA = 8;

reg [8:0] x_ff;
reg [6:0] y_ff;
reg [5:0] r_ff;
reg [5:0] g_ff;
reg [5:0] b_ff;
always @(posedge clk) begin
    if (rst) begin
        state_ff <= ST_INIT;
    end else begin
        case (state_ff)
            ST_INIT: begin
                x_ff <= x_i;
                y_ff <= y_i;
                if (pop_i) begin
                    state_ff <= ST_EMIT_CMD;
                end
            end
            ST_EMIT_CMD: begin
                state_ff <= ST_WAIT_DATA;
            end
            ST_WAIT_DATA: begin
                if (mig_rd_count == 6'h00) begin // FIXME: mig_rd_empty?
                    state_ff <= ST_WAIT_DATA;
                end else begin
                    r_ff <= mig_rd_data[21:16];
                    g_ff <= mig_rd_data[13:8];
                    b_ff <= mig_rd_data[5:0];
                    state_ff <= ST_PUSH_DATA;
                end
            end
            ST_PUSH_DATA: begin
                state_ff <= ST_INIT;
            end
        endcase
    end
end

assign mig_cmd_clk = clk;
assign mig_cmd_en = (state_ff == ST_EMIT_CMD) ? 1'b1 : 1'b0;
assign mig_cmd_instr = 3'b001;
assign mig_cmd_bl = 6'h00;
assign mig_cmd_byte_addr[29:0] = {12'h000, y_ff, x_ff, 2'b00};
// FIXME: wait until mig_cmd_empty or !mig_cmd_full?

assign mig_wr_clk = clk;
assign mig_wr_en = 1'b0;
assign mig_wr_mask = 4'b0000;
assign mig_wr_data = 32'h0000;
// mig_wr_full, mig_wr_empty, mig_wr_count, mig_wr_underrun, mig_wr_error

assign mig_rd_clk = clk;
assign mig_rd_en = (state_ff == ST_WAIT_DATA) ? 1'b1 : 1'b0;
// mig_rd_full, mig_rd_empty, mig_rd_overflow, mig_rd_error

assign r_o = r_ff;
assign g_o = g_ff;
assign b_o = b_ff;
assign ack_o = (state_ff == ST_PUSH_DATA) ? 1'b1 : 1'b0;

endmodule
`default_nettype wire
