`default_nettype none

module csr_cmd_decoder(
    input [7:0] cmd,
    output we,
    output target_nkmdprom,
    output target_csr,
    output [3:0] addr_high);

assign we = cmd[7];

wire [1:0] target;
assign target = cmd[5:4];
assign target_nkmdprom = (target == 2'b01);
assign target_csr = (target == 2'b00);

assign addr_high = cmd[3:0];

endmodule

module csr_spi #(
    parameter NUM_CH = 8,
    parameter NUM_SPDIF_IN = 3,
    parameter NUM_RATE = 5,

    parameter VOL_WIDTH = NUM_CH*32,
    parameter NKMDDBG_WIDTH = 16*8,
    parameter RATE_WIDTH = NUM_SPDIF_IN*NUM_RATE,
    parameter UDATA_WIDTH = NUM_SPDIF_IN*192,
    parameter CDATA_WIDTH = UDATA_WIDTH
)(
    input clk,
    input rst,

    // spi access
    input sck,
    output miso,
    input mosi,
    input ss,

    // csr registers access
    output [(VOL_WIDTH-1):0] vol_o,
    output nkmd_rst_o,
    input [(NKMDDBG_WIDTH-1):0] nkmd_dbgout_i,
    output [(NKMDDBG_WIDTH-1):0] nkmd_dbgin_o,
    input [(RATE_WIDTH-1):0] rate_i,
    input [(UDATA_WIDTH-1):0] udata_i,
    input [(CDATA_WIDTH-1):0] cdata_i,

    // nkmd prom
    output [31:0] prom_addr_o,
    output [31:0] prom_data_o,
    output prom_ack_o);

wire spi_rst;
wire [7:0] spi_data_rx;
wire spi_ack_pop_o;
wire spi_ack_i;
wire [7:0] spi_data_tx;

spi_trx spi_trx(
    .clk(clk),
    .sck(sck), .miso(miso), .mosi(mosi), .ss(ss),
    .rst_o(spi_rst),
    .data_o(spi_data_rx), .ack_pop_o(spi_ack_pop_o),
    .data_i(spi_data_tx), .ack_i(spi_ack_i));

wire dec_we;
wire dec_target_nkmdprom;
wire dec_target_csr;
wire [3:0] dec_addr_high;
csr_cmd_decoder csr_cmd_decoder(
    .cmd(spi_data_rx),
    .we(dec_we), .target_nkmdprom(dec_target_nkmdprom), .target_csr(dec_target_csr),
    .addr_high(dec_addr_high));

reg cmd_we_ff;
reg [19:0] addr_ff;
reg [31:0] wdata_ff;

wire [7:0] csr_data_o;

reg [15:0] state_ff;
parameter ST_INIT = 0;
parameter ST_PENDING_CSR_ADDR = 1;
parameter ST_PENDING_CSR_DATA = 2;
parameter ST_RESPOND_CSR_DATA = 3;
parameter ST_PENDING_NKMDPROM_ADDR_MID = 4;
parameter ST_PENDING_NKMDPROM_ADDR_LOW = 5;
parameter ST_PENDING_NKMDPROM_DATA = 6;
parameter ST_WRITING_NKMDPROM_DATA = 7;
reg [1:0] data_offset_ff; // For 32bit targets, this reg will keep which wdata_ff octet needs to be filled in next

reg [7:0] data_tx_ff;
assign spi_data_tx = data_tx_ff;
reg data_ready_ff;
assign spi_ack_i = data_ready_ff;

always @(posedge clk) begin
    if (rst || spi_rst) begin
        state_ff <= ST_INIT;
        data_tx_ff <= 8'h00;
        data_ready_ff <= 0;

        cmd_we_ff <= 1'b0;
        addr_ff <= 20'b0;
        wdata_ff <= 32'b0;
        data_offset_ff <= 2'h0;
    end else begin
        case (state_ff)
            ST_INIT: begin
                if (spi_ack_pop_o) begin
                    addr_ff <= {dec_addr_high, 16'h0};
                    cmd_we_ff <= dec_we;

                    data_tx_ff <= 8'hcc;
                    data_ready_ff <= 1;

                    if (dec_target_csr)
                        state_ff <= ST_PENDING_CSR_ADDR;
                    else if (dec_target_nkmdprom)
                        state_ff <= ST_PENDING_NKMDPROM_ADDR_MID;
                    else
                        state_ff <= ST_INIT;
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_INIT;
                end
            end
            ST_PENDING_CSR_ADDR: begin
                if (spi_ack_pop_o) begin
                    addr_ff <= {addr_ff[20:16], spi_data_rx[7:0], 8'h0};

                    data_tx_ff <= 8'had;
                    data_ready_ff <= 1;

                    state_ff <= ST_PENDING_CSR_DATA;
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_PENDING_CSR_ADDR;
                end
            end
            ST_PENDING_CSR_DATA: begin
                data_ready_ff <= 0;
                if (spi_ack_pop_o) begin
                    state_ff <= ST_RESPOND_CSR_DATA;
                end else begin
                    state_ff <= ST_PENDING_CSR_DATA;
                end
            end
            ST_RESPOND_CSR_DATA: begin
                data_tx_ff <= csr_data_o;
                data_ready_ff <= 1;

                addr_ff[19:8] <= addr_ff[19:8] + 1;
                state_ff <= ST_PENDING_CSR_DATA;
            end
            ST_PENDING_NKMDPROM_ADDR_MID: begin
                if (spi_ack_pop_o) begin
                    data_tx_ff <= 8'ha0;
                    data_ready_ff <= 1;

                    addr_ff <= {addr_ff[19:16], spi_data_rx[7:0], 8'b0};

                    state_ff <= ST_PENDING_NKMDPROM_ADDR_LOW;
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_PENDING_NKMDPROM_ADDR_MID;
                end
            end
            ST_PENDING_NKMDPROM_ADDR_LOW: begin
                if (spi_ack_pop_o) begin
                    data_tx_ff <= 8'ha1;
                    data_ready_ff <= 1;

                    addr_ff <= {addr_ff[19:16], addr_ff[15:8], spi_data_rx[7:0]};

                    state_ff <= ST_PENDING_NKMDPROM_DATA;
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_PENDING_NKMDPROM_ADDR_LOW;
                end
            end
            ST_PENDING_NKMDPROM_DATA: begin
                if (spi_ack_pop_o) begin
                    data_tx_ff <= {4'hd, 2'b00, data_offset_ff};
                    data_ready_ff <= 1;

                    wdata_ff <= {wdata_ff[23:0], spi_data_rx[7:0]};
                    if (cmd_we_ff == 1'b1 && data_offset_ff == 4'h3)
                        state_ff <= ST_WRITING_NKMDPROM_DATA;
                    else
                        state_ff <= ST_PENDING_NKMDPROM_DATA;

                    data_offset_ff <= data_offset_ff + 1;
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_PENDING_NKMDPROM_DATA;
                end
            end
            ST_WRITING_NKMDPROM_DATA: begin
                data_ready_ff <= 0;
                state_ff <= ST_PENDING_NKMDPROM_DATA;

                addr_ff <= addr_ff + 1;
            end
            default: begin
                data_ready_ff <= 0;
                state_ff <= ST_INIT;
            end
        endcase
    end
end

wire [11:0] csr_addr = addr_ff[19:8];
wire csr_ack_i = state_ff == ST_PENDING_CSR_DATA && spi_ack_pop_o == 1'b1 && cmd_we_ff == 1'b1;
csr #(.NUM_CH(NUM_CH), .NUM_SPDIF_IN(NUM_SPDIF_IN)) csr(
    .clk(clk), .rst(rst),
    .addr_i(csr_addr), .ack_i(csr_ack_i), .data_i(spi_data_rx), .data_o(csr_data_o),
    .vol_o(vol_o),
    .nkmd_rst_o(nkmd_rst_o), .nkmd_dbgout_i(nkmd_dbgout_i), .nkmd_dbgin_o(nkmd_dbgin_o),
    .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i));

wire prom_addr_o = {12'b0, addr_ff[19:0]};
wire prom_data_o = wdata_ff;
wire prom_ack_o = (state_ff == ST_WRITING_NKMDPROM_DATA);

endmodule
