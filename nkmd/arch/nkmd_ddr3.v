`default_nettype none
module nkmd_ddr3(
    input wire clk,
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

    // To nkmm bus
    input wire [31:0] data_i,
    output wire [31:0] data_o,
    input wire [15:0] addr_i,
    input wire we_i);

localparam SPAD_OFFSET = 4'h1;

reg [31:0] spad [1023:0];

wire [9:0] offset_i;
assign offset_i = addr_i[9:0];

reg [24:0] dma_dram_word_addr_ff;
reg [9:0] dma_spad_addr_ff;
reg [5:0] dma_burst_len_ff;
wire dma_complete;

reg [31:0] out_ff;
always @(posedge clk) begin
    if (addr[15:12] == SPAD_OFFSET) begin
        if (we_i)
            spad[offset_i] <= data_i;

        out_ff <= spad[offset_i];
    end else if (addr_i == 16'hc100) begin
        if (we_i)
            dma_dram_addr_ff <= data_i[26:2];

        out_ff <= {5'b00000, dma_dram_addr_ff, 2'b00};
    end else if (addr_i == 16'hc101) begin
        if (we_i)
            dma_spad_addr_ff <= data_i[9:0];

        out_ff <= {12'b0, dma_spad_len_ff};
    end else if (addr_i == 16'hc102) begin
        if (we_i)
            dma_burst_len_ff <= data_i[5:0];

        out_ff <= {26'b0, dma_burst_len_ff};
    end else if (addr_i == 16'hc103) begin
        out_ff <= {31'b0, dma_complete};
    end else begin
        out_ff <= 32'h0;
    end
end

reg [5:0] dma_state_ff;
localparam ST_INIT = 0;
localparam ST_FILL_WRBUF = 1;
localparam ST_EMIT_WR_CMD = 2;
localparam ST_EMIT_RD_CMD = 3;
localparam ST_WAIT_RD_DATA = 4;

reg [5:0] dma_burst_left_ff;
always @(posedge clk) begin
    if (rst) begin
        dma_state_ff <= ST_INIT;
    end else begin
        case (dma_state_ff) begin
            ST_INIT: begin
                dma_burst_left_ff <= dma_burst_len_ff;
                if (we_i && addr_i == 16'hc103) begin
                    if (data_i[0] == 1'b1) begin
                        dma_spad_addr_ff <= dma_spad_addr_ff + 1;
                        dma_state_ff <= ST_FILL_WRBUF;
                    end else begin
                        dma_state_ff <= ST_EMIT_RD_CMD;
                    end
                end
            end
            ST_FILL_WRBUF: begin
                dma_spad_addr_ff <= dma_spad_addr_ff + 1;
                dma_burst_left_ff <= dma_burst_left_ff - 1;
                if (dma_burst_left_ff != 6'h00) begin
                    dma_state_ff <= ST_FILL_WRBUF;
                end else begin
                    dma_state_ff <= ST_EMIT_WR_CMD;
                end
            end
            ST_EMIT_WR_CMD: begin
                dma_state_ff <= ST_INIT;
            end
            ST_EMIT_RD_CMD: begin
                dma_state_ff <= ST_WAIT_RD_DATA;
            end
            ST_WAIT_RD_DATA: begin
                if (mig_rd_count == 6'h00) begin
                    dma_state_ff <= ST_WAIT_RD_DATA;
                end else begin
                    dma_spad_addr_ff <= dma_spad_addr_ff + 1;
                    dma_burst_left_ff <= dma_burst_left_ff - 1;
                    spad[dma_spad_addr_ff] <= mig_rd_data;
                    if (dma_burst_left_ff != 6'h00) begin
                        dma_state_ff <= ST_WAIT_RD_DATA;
                    end else begin
                        dma_state_ff <= ST_INIT;
                    end
                end
            end
        endcase
    end
end
assign dma_complete = dma_state_ff == ST_INIT;

assign mig_cmd_clk = clk;
assign mig_cmd_en = (dma_state_ff == ST_EMIT_RD_CMD || dma_state_ff == ST_EMIT_WR_CMD) ? 1'b1 : 1'b0;
assign mig_cmd_instr = dma_state_ff == ST_EMIT_RD_CMD ? 3'b011 : 3'b010;
assign mig_cmd_bl = dma_burst_len_ff;
assign mig_cmd_byte_addr[29:0] = {3'b000, dma_dram_addr_ff, 2'b00};
// FIXME: wait until mig_cmd_empty or !mig_cmd_full?

assign mig_wr_clk = clk;
assign mig_wr_en = (dma_state_ff == ST_FILL_WRBUF);
assign mig_wr_mask = 4'b0000;
reg [31:0] mig_wr_data_ff;
always @(posedge clk)
    mig_wr_data_ff <= spad[dma_spad_addr_ff];
assign mig_wr_data = mig_wr_data_ff;
// mig_wr_full, mig_wr_empty, mig_wr_count, mig_wr_underrun, mig_wr_error

assign mig_rd_clk = clk;
assign mig_rd_en = dma_state_ff == ST_WAIT_RD_DATA ? 1'b1 : 1'b0;
// mig_rd_full, mig_rd_empty, mig_rd_overflow, mig_rd_error

endmodule
`default_nettype wire
