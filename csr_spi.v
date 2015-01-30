module csr_cmd_decoder(
    input [7:0] cmd,
    output [7:0] nrep,
    output we,
    output target_csr,
    output [3:0] ch,
    output nop);

function [7:0] decode_nrep(input [1:0] encoded);
    case (encoded)
    2'b00: decode_nrep = 8'd0;
    2'b01: decode_nrep = 8'd3;
    2'b10: decode_nrep = 8'd63;
    2'b11: decode_nrep = 8'd255;
    endcase
endfunction

assign nrep = decode_nrep(cmd[7:6]);
assign we = cmd[5];
assign target_csr = cmd[4];
assign ch = cmd[3:0];

assign nop = cmd[7:6] == 2'b00 && target_csr == 2'b0;

endmodule

module csr_spi #(
    parameter NUM_CH = 8,
    parameter NUM_SPDIF_IN = 3,
    parameter NUM_RATE = 5,

    parameter VOL_WIDTH = NUM_CH*32,
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

    // registers access
    output [(VOL_WIDTH-1):0] vol_o, // addr: 12'h000 ~
    input [(RATE_WIDTH-1):0] rate_i, // addr: 12'h800 ~
    input [(UDATA_WIDTH-1):0] udata_i, // addr: 12'h900 ~
    input [(CDATA_WIDTH-1):0] cdata_i  // addr: 12'ha00 ~
    );

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

wire [7:0] dec_nrep;
wire dec_we;
wire dec_target_csr;
wire [3:0] dec_ch;
wire dec_nop;
csr_cmd_decoder csr_cmd_decoder(
    .cmd(spi_data_rx),
    .nrep(dec_nrep), .we(dec_we), .target_csr(dec_target_csr), .ch(dec_ch), .nop(dec_nop));

reg [7:0] nrep_counter;
reg cmd_we_ff;
reg [3:0] cmd_ch_ff;
reg [7:0] csr_addr_low_ff;

wire [7:0] csr_data_o;

reg [15:0] state_ff;
parameter ST_INIT = 0;
parameter ST_PENDING_CSR_ADDR = 1;
parameter ST_PENDING_CSR_DATA = 2;
parameter ST_RESPOND_CSR_DATA = 3;
parameter ST_PENDING_XCHG_DATA = 4;

// FIXME: split always for data_tx
reg [7:0] data_tx_ff;
assign spi_data_tx = data_tx_ff;
reg data_ready_ff;
assign spi_ack_i = data_ready_ff;

always @(posedge clk) begin
    data_ready_ff <= 0;

    if(rst) begin
        state_ff <= ST_INIT;
        data_tx_ff <= 8'h00;
    end else begin
        case (state_ff)
            ST_INIT: begin
                if (spi_ack_pop_o) begin
                    nrep_counter <= dec_nrep;        
                    cmd_ch_ff <= dec_ch;
                    cmd_we_ff <= dec_we;

                    data_tx_ff <= 8'hcc;
                    data_ready_ff <= 1;

                    if (dec_nop)
                        state_ff <= ST_INIT;
                    else if (dec_target_csr)
                        state_ff <= ST_PENDING_CSR_ADDR;
                    else
                        state_ff <= ST_PENDING_XCHG_DATA;
                end else
                    state_ff <= ST_INIT;
            end
            ST_PENDING_CSR_ADDR: begin
                if (spi_ack_pop_o) begin
                    csr_addr_low_ff[7:0] <= spi_data_rx[7:0];

                    data_tx_ff <= 8'had;
                    data_ready_ff <= 1;

                    state_ff <= ST_PENDING_CSR_DATA;
                end else
                    state_ff <= ST_PENDING_CSR_ADDR;
            end
            ST_PENDING_CSR_DATA: begin
                if (spi_ack_pop_o) begin
                    state_ff <= ST_RESPOND_CSR_DATA;
                end else
                    state_ff <= ST_PENDING_CSR_DATA;
            end
            ST_RESPOND_CSR_DATA: begin
                data_tx_ff <= csr_data_o;
                data_ready_ff <= 1;

                csr_addr_low_ff <= csr_addr_low_ff + 1;

                nrep_counter <= nrep_counter - 1;
                if (nrep_counter != 0)
                    state_ff <= ST_PENDING_CSR_DATA;
                else
                    state_ff <= ST_INIT;
            end
            //ST_PENDING_XCHG_DATA: // unimpl
            default:
                state_ff <= ST_INIT;
        endcase
    end
end

wire [11:0] csr_addr = {cmd_ch_ff, csr_addr_low_ff};
wire csr_ack_i = state_ff == ST_PENDING_CSR_DATA && spi_ack_pop_o == 1'b1 && cmd_we_ff == 1'b1;
csr #(.NUM_CH(NUM_CH), .NUM_SPDIF_IN(NUM_SPDIF_IN)) csr(
    .clk(clk), .rst(rst),
    .addr_i(csr_addr), .ack_i(csr_ack_i), .data_i(spi_data_rx), .data_o(csr_data_o),
    .vol_o(vol_o), .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i));

endmodule
