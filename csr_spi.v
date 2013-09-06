module csr_spi#(
    parameter NUM_CH = 4,
    parameter VOL_WIDTH = NUM_CH*2*16,
    parameter RATE_WIDTH = NUM_CH*4,
    parameter UDATA_WIDTH = NUM_CH*192,
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
    input [(RATE_WIDTH-1):0] rate_i,  // addr: 12'800 ~
    input [(UDATA_WIDTH-1):0] udata_i,  // addr: 12'900 ~
    input [(CDATA_WIDTH-1):0] cdata_i  // addr: 12'a00 ~
    );

wire spi_rst;
wire [7:0] spi_data_rx;
wire spi_ack_pop_o;
wire [7:0] spi_data_tx;
reg spi_ack_i_ff;

spi_trx spi_trx(
    .clk(clk),
    .sck(sck), .miso(miso), .mosi(mosi), .ss(ss),
    .rst_o(spi_rst),
    .data_o(spi_data_rx), .ack_pop_o(spi_ack_pop_o),
    .data_i(spi_data_tx), .ack_i(spi_ack_i_ff));

reg [11:0] csr_addr_ff;
reg csr_we_ff;
reg [7:0] csr_data_w_ff;
wire [7:0] csr_data_o;

csr #(.NUM_CH(NUM_CH)) csr(
    .clk(clk), .rst(rst),
    .addr_i(csr_addr_ff), .ack_i(csr_we_ff), .data_i(csr_data_w_ff), .data_o(csr_data_o),
    .vol_o(vol_o), .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i));
assign spi_data_tx = csr_data_o;

reg csr_addr_inc_ff;

parameter ST_INIT = 0;
parameter ST_W_ADDR_IN = 1;
parameter ST_W_DATA_IN = 2;
parameter ST_R_ADDR_IN = 3;
parameter ST_R_DATA_OUT_CONT = 4;
reg [2:0] state;
always @(posedge clk) begin
    spi_ack_i_ff <= 0;
    csr_we_ff <= 0;
    csr_addr_inc_ff <= 0;

    if(rst || spi_rst) begin
        state <= ST_INIT;
        csr_addr_ff <= 0;
        csr_data_w_ff <= 0;
    end else if(spi_ack_pop_o) begin
        case(state)
            ST_INIT: begin
                csr_addr_ff[11:8] <= spi_data_rx[3:0];

                if(spi_data_rx[7])
                    state <= ST_W_ADDR_IN;
                else
                    state <= ST_R_ADDR_IN;
            end
            ST_W_ADDR_IN: begin
                csr_addr_ff[7:0] <= spi_data_rx[7:0];
                state <= ST_W_DATA_IN;
            end
            ST_W_DATA_IN: begin
                csr_data_w_ff <= spi_data_rx;
                csr_addr_inc_ff <= 1;
                csr_we_ff <= 1;
            end
            ST_R_ADDR_IN: begin
                csr_addr_ff[7:0] <= spi_data_rx[7:0];
                spi_ack_i_ff <= 1;

                state <= ST_R_DATA_OUT_CONT;
            end
            ST_R_DATA_OUT_CONT: begin
                csr_addr_inc_ff <= 1;
                spi_ack_i_ff <= 1;
            end
            default:
                state <= ST_INIT;
        endcase
    end
end

endmodule
