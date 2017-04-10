`default_nettype none
module simple_ddr3(
    input wire clk, // clk491520
    input wire rst,

    input wire [27:0] addr_i,
    input wire [31:0] data_i,
    input wire we_i,
    input wire pop_i,
    output wire [31:0] data_o,
    output wire ack_o,
    output wire busy_o,

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
    input wire mig_rd_error);

reg [27:0] addr_ff;
reg [31:0] wdata_ff;
reg [31:0] rdata_ff;
reg ack_ff;

reg [5:0] state_ff;
localparam ST_INIT = 1;
localparam ST_FILL_WRBUF = 2;
localparam ST_EMIT_WR_CMD = 3;
localparam ST_EMIT_RD_CMD = 4;
localparam ST_WAIT_RD_DATA = 5;

always @(posedge clk) begin
    if (rst) begin
        addr_ff <= 28'b0;
        wdata_ff <= 32'b0;
        rdata_ff <= 32'b0;
        state_ff <= ST_INIT;
        ack_ff <= 1'b0;
    end else begin
        case (state_ff)
            ST_INIT: begin
                addr_ff <= addr_i;
                wdata_ff <= data_i;
                ack_ff <= 1'b0;

                if (we_i) begin
                    state_ff <= ST_FILL_WRBUF;
                end else if (pop_i) begin
                    state_ff <= ST_EMIT_RD_CMD;
                end else begin
                    state_ff <= ST_INIT;
                end
            end
            ST_FILL_WRBUF: begin
                state_ff <= ST_EMIT_WR_CMD;
                ack_ff <= 1'b0;
            end
            ST_EMIT_WR_CMD: begin
                state_ff <= ST_INIT;
                ack_ff <= 1'b0;
            end
            ST_EMIT_RD_CMD: begin
                state_ff <= ST_WAIT_RD_DATA;
                ack_ff <= 1'b0;
            end
            ST_WAIT_RD_DATA: begin
                if (mig_rd_count == 6'h00) begin // FIXME: mig_rd_empty?
                    state_ff <= ST_WAIT_RD_DATA;
                    ack_ff <= 1'b0;
                end else begin
                    rdata_ff <= mig_rd_data;
                    state_ff <= ST_INIT;
                    ack_ff <= 1'b1;
                end
            end
        endcase
    end
end

assign mig_cmd_clk = clk;
assign mig_cmd_en = (state_ff == ST_EMIT_RD_CMD || state_ff == ST_EMIT_WR_CMD) ? 1'b1 : 1'b0;
assign mig_cmd_instr = (state_ff == ST_EMIT_RD_CMD) ? 3'b001 : 3'b010;
assign mig_cmd_bl = 6'b0;
assign mig_cmd_byte_addr[29:0] = {addr_ff, 2'b00};
// FIXME: wait until mig_cmd_empty or !mig_cmd_full?

assign mig_wr_clk = clk;
assign mig_wr_en = (state_ff == ST_FILL_WRBUF) ? 1'b1 : 1'b0;
assign mig_wr_mask = 4'b0000;
assign mig_wr_data = wdata_ff;
// mig_wr_full, mig_wr_empty, mig_wr_count, mig_wr_underrun, mig_wr_error

assign mig_rd_clk = clk;
assign mig_rd_en = (state_ff == ST_WAIT_RD_DATA) ? 1'b1 : 1'b0;
// mig_rd_full, mig_rd_empty, mig_rd_overflow, mig_rd_error

assign data_o = rdata_ff;
assign ack_o = ack_ff;
assign busy_o = (state_ff != ST_INIT) ? 1'b1 : 1'b0;

endmodule
`default_nettype wire
