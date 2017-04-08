`default_nettype none

module csr_cmd_decoder(
    input wire [7:0] cmd,
    output wire nop,
    output wire special,
    output wire we,
    output wire [7:0] nrep,
    output wire target_nkmdprom,
    output wire target_csr,
    output wire [3:0] addr_high);

assign we = cmd[7];

function [7:0] decode_nrep(
    input [1:0] enc);
begin
    case (enc)
        2'b00: decode_nrep = 8'd0;
        2'b01: decode_nrep = 8'd1;
        2'b10: decode_nrep = 8'd4;
        2'b11: decode_nrep = 8'd16;
    endcase
end
endfunction

assign nrep = decode_nrep(cmd[6:5]);

assign nop = cmd[6:5] == 2'b00 && cmd[3:0] != 4'hf;
assign target_nkmdprom = cmd[4] == 1'b1;
assign target_csr = cmd[4] == 1'b0;
assign special = cmd[6:5] == 2'b00 && cmd[3:0] == 4'hf;

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
    input wire clk,
    input wire rst,

    // spi access
    input wire sck,
    output wire miso,
    input wire mosi,
    input wire ss,

    // csr registers access
    output wire [(VOL_WIDTH-1):0] vol_o,
    output wire nkmd_rst_o,
    input wire [(NKMDDBG_WIDTH-1):0] nkmd_dbgout_i,
    output wire [(NKMDDBG_WIDTH-1):0] nkmd_dbgin_o,
    input wire [(RATE_WIDTH-1):0] rate_i,
    input wire [(UDATA_WIDTH-1):0] udata_i,
    input wire [(CDATA_WIDTH-1):0] cdata_i,

    // nkmd prom
    output wire [31:0] prom_addr_o,
    output wire [31:0] prom_data_o,
    output wire prom_ack_o,

    // dram peek/poke 
    output wire [27:0] dram0_addr_o,
    output wire [31:0] dram0_data_o,
    output wire dram0_we_o,
    output wire dram0_pop_o,
    input wire [31:0] dram0_data_i,
    input wire dram0_ack_i);

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

wire dec_nop;
wire dec_special;
wire dec_we;
wire dec_target_nkmdprom;
wire dec_target_csr;
wire [7:0] dec_nrep;
wire [3:0] dec_addr_high;
csr_cmd_decoder csr_cmd_decoder(
    .cmd(spi_data_rx),
    .nop(dec_nop),
    .special(dec_special),
    .we(dec_we),
    .nrep(dec_nrep),
    .target_nkmdprom(dec_target_nkmdprom), .target_csr(dec_target_csr),
    .addr_high(dec_addr_high));

reg cmd_we_ff;
reg [7:0] nrep_ff;
reg [19:0] addr_ff;
reg [31:0] sp_addr_ff;
reg [31:0] rdata_ff;
reg [31:0] wdata_ff;

wire [11:0] csr_addr;
wire csr_ack_i;
wire [7:0] csr_data_o;

reg [15:0] state_ff;
parameter ST_INIT = 0;
parameter ST_SPECIAL = 1;
parameter ST_PENDING_CSR_ADDR = 2;
parameter ST_PENDING_CSR_DATA = 3;
parameter ST_RESPOND_CSR_DATA = 4;
parameter ST_PENDING_NKMDPROM_ADDR_MID = 5;
parameter ST_PENDING_NKMDPROM_ADDR_LOW = 6;
parameter ST_PENDING_NKMDPROM_DATA = 7;
parameter ST_WRITING_NKMDPROM_DATA = 8;
parameter ST_SP_PENDING_ADDR = 9;
parameter ST_SP_READ_DATA = 10;
parameter ST_SP_READ_WAIT_ACK = 11;
parameter ST_SP_PENDING_DATA = 12;
parameter ST_SP_WRITE_DATA = 13;
reg [1:0] addr_offset_ff; // For 32bit addrs, this reg will keep which sp_addr_ff octet needs to be filled in next
reg [1:0] data_offset_ff; // For 32bit targets, this reg will keep which wdata_ff octet needs to be filled in next

reg [7:0] data_tx_ff;
assign spi_data_tx = data_tx_ff;
reg data_ready_ff;
assign spi_ack_i = data_ready_ff;

always @(posedge clk) begin
    if (rst) begin
        state_ff <= ST_INIT;
        data_tx_ff <= 8'h00;
        data_ready_ff <= 0;

        cmd_we_ff <= 1'b0;
        addr_ff <= 20'b0;
        sp_addr_ff <= 32'b0;
        wdata_ff <= 32'b0;
        data_offset_ff <= 2'h0;
    end else begin
        case (state_ff)
            ST_INIT: begin
                if (spi_ack_pop_o) begin
                    cmd_we_ff <= dec_we;
                    nrep_ff <= dec_nrep;
                    addr_ff <= {dec_addr_high, 16'h0};

                    data_ready_ff <= 1;

                    if (dec_nop) begin
                        data_tx_ff <= 8'h90;
                        state_ff <= ST_INIT;
                    end else if (dec_special) begin
                        data_tx_ff <= 8'h91;
                        state_ff <= ST_SPECIAL;
                    end else if (dec_target_csr) begin
                        data_tx_ff <= 8'hcc;
                        state_ff <= ST_PENDING_CSR_ADDR;
                    end else if (dec_target_nkmdprom) begin
                        data_tx_ff <= 8'hca;
                        state_ff <= ST_PENDING_NKMDPROM_ADDR_MID;
                    end else begin
                        /* NOT REACHED */
                        data_tx_ff <= 8'h90;
                        state_ff <= ST_INIT;
                    end
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_INIT;
                end
            end
            ST_SPECIAL: begin
                if (spi_ack_pop_o) begin
                    cmd_we_ff <= dec_we;
                    nrep_ff <= dec_nrep;

                    addr_offset_ff <= 2'h3;
                    state_ff <= ST_SP_PENDING_ADDR;

                    data_tx_ff <= 8'hc0;
                    data_ready_ff <= 1;
                end
            end
            ST_PENDING_CSR_ADDR: begin
                if (spi_ack_pop_o) begin
                    addr_ff <= {addr_ff[19:16], spi_data_rx[7:0], 8'h0};

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

                if (nrep_ff != 8'h01) begin
                    nrep_ff <= nrep_ff - 1;
                    state_ff <= ST_PENDING_CSR_DATA;
                end else begin
                    state_ff <= ST_INIT;
                end
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
                    data_ready_ff <= 1'b1;

                    wdata_ff <= {wdata_ff[23:0], spi_data_rx[7:0]};
                    if (data_offset_ff == 2'h3)
                        state_ff <= ST_WRITING_NKMDPROM_DATA;
                    else
                        state_ff <= ST_PENDING_NKMDPROM_DATA;

                    data_offset_ff <= data_offset_ff + 1;
                end else begin
                    data_ready_ff <= 1'b0;
                    state_ff <= ST_PENDING_NKMDPROM_DATA;
                end
            end
            ST_WRITING_NKMDPROM_DATA: begin
                data_ready_ff <= 1'b0;

                addr_ff <= addr_ff + 1;
                if (nrep_ff != 8'h01) begin
                    nrep_ff <= nrep_ff - 1;
                    state_ff <= ST_PENDING_NKMDPROM_DATA;
                end else begin
                    state_ff <= ST_INIT;
                end
            end
            ST_SP_PENDING_ADDR: begin
                if (spi_ack_pop_o) begin
                    data_tx_ff <= {4'ha, 2'b00, addr_offset_ff};
                    data_ready_ff <= 1;

                    sp_addr_ff <= {sp_addr_ff[23:0], spi_data_rx[7:0]};
                    addr_offset_ff <= addr_offset_ff - 1; 

                    if (addr_offset_ff != 4'h0) begin
                        state_ff <= ST_SP_PENDING_ADDR;
                    end else begin
                        state_ff <= ST_SP_READ_DATA;
                    end
                end else begin
                    data_ready_ff <= 1'b0
                    state_ff <= ST_SP_PENDING_ADDR;
                end
            end
            ST_SP_READ_DATA: begin
                state_ff <= ST_SP_READ_WAIT_ACK;
            end
            ST_SP_READ_WAIT_ACK: begin
                if (dram0_ack_i) begin
                    rdata_ff <= dram0_data_i;
                    state_ff <= ST_SP_PENDING_DATA;
                end else begin
                    state_ff <= ST_SP_READ_WAIT_ACK;
                end
            end
            ST_SP_PENDING_DATA: begin
                if (spi_ack_pop_o) begin
                    wdata_ff <= {wdata_ff[23:0], spi_data_rx[7:0]};
                    if (data_offset_ff == 4'h3)
                        state_ff <= ST_SP_WRITE_DATA;
                    else
                        state_ff <= ST_SP_PENDING_DATA;

                    case (data_offset_ff)
                        2'h0: data_tx_ff <= rdata_ff[7:0];
                        2'h1: data_tx_ff <= rdata_ff[15:8];
                        2'h2: data_tx_ff <= rdata_ff[23:16];
                        2'h3: data_tx_ff <= rdata_ff[31:24];
                    endcase
                    data_ready_ff <= 1;

                    data_offset_ff <= data_offset_ff + 1;
                end else begin
                    data_ready_ff <= 0;
                    state_ff <= ST_SP_PENDING_DATA;
                end
            end
            ST_WRITE_DATA: begin
                data_ready_ff <= 1'b0;

                addr_ff <= addr_ff + 1;
                if (nrep_ff != 8'h01) begin
                    nrep_ff <= nrep_ff - 1;
                    state_ff <= ST_SP_READ_DATA;
                end else begin
                    state_ff <= ST_INIT;
                end
            end
            default: begin
                data_ready_ff <= 0;
                state_ff <= ST_INIT;
            end
        endcase
    end
end

csr #(.NUM_CH(NUM_CH), .NUM_SPDIF_IN(NUM_SPDIF_IN)) csr(
    .clk(clk), .rst(rst),
    .addr_i(csr_addr), .ack_i(csr_ack_i), .data_i(spi_data_rx), .data_o(csr_data_o),
    .vol_o(vol_o),
    .nkmd_rst_o(nkmd_rst_o), .nkmd_dbgout_i(nkmd_dbgout_i), .nkmd_dbgin_o(nkmd_dbgin_o),
    .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i));
assign csr_addr = addr_ff[19:8];
assign csr_ack_i = state_ff == ST_PENDING_CSR_DATA && spi_ack_pop_o == 1'b1 && cmd_we_ff == 1'b1;

assign prom_addr_o = {12'b0, addr_ff[19:0]};
assign prom_data_o = wdata_ff;
assign prom_ack_o = (state_ff == ST_WRITING_NKMDPROM_DATA);

assign dram0_addr_o = sp_addr_ff[27:0];
assign dram0_data_o = wdata_ff;
assign dram0_we_o = (cmd_we_ff == 1'b1 && state_ff == ST_SP_WRITE_DATA) ? 1'b1 : 1'b0;
assign dram0_pop_o = (state_ff == ST_SP_READ_DATA) ? 1'b1 : 1'b0;

endmodule

`default_nettype wire
