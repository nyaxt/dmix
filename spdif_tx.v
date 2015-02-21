module spdif_tx(
    input clk, // 24.576Mhz
    input rst,

    input [1:0] ack_i,
	input [47:0] data_i,
    output [1:0] pop_o,

    output spdif_o
);

reg halfbit_ff;
always @(posedge clk) begin
    if (rst)
        halfbit_ff <= 1'b0;
    else
        halfbit_ff <= ~halfbit_ff;
end
wire halfbit = halfbit_ff;

reg [47:0] data_latch;
always @(posedge clk) begin
    if (ack_i[0])
        data_latch[23:0] <= data_i[23:0];
    if (ack_i[1])
        data_latch[47:24] <= data_i[47:24];
end

parameter SYNCCODE_B0 = 8'b00010111;
parameter SYNCCODE_W0 = 8'b00011011;
parameter SYNCCODE_M0 = 8'b00011101;
parameter SYNCCODE_B1 = ~SYNCCODE_B0;
parameter SYNCCODE_W1 = ~SYNCCODE_W0;
parameter SYNCCODE_M1 = ~SYNCCODE_M0;

reg [4:0] subframe_pos_counter_ff;
always @(posedge clk) begin
    if (rst) begin
        subframe_pos_counter_ff <= 5'd0;
    end else if (halfbit) begin
        subframe_pos_counter_ff <= subframe_pos_counter_ff + 5'd1;
    end
end
wire send_synccode = subframe_pos_counter_ff < 5'd4;
wire prepare_subframe = halfbit & (subframe_pos_counter_ff == 5'd3);
wire prepare_synccode_type = ~halfbit & (subframe_pos_counter_ff == 5'd31);
wire prepare_synccode = halfbit & (subframe_pos_counter_ff == 5'd31);

wire phase;

reg [2:0] synccode_type_ff;
parameter SYNCCODE_TYPE_B = 0;
parameter SYNCCODE_TYPE_W = 1;
parameter SYNCCODE_TYPE_M = 2;
reg [7:0] frame_counter_ff;
always @(posedge clk) begin
    if (rst) begin
        synccode_type_ff <= SYNCCODE_TYPE_B;
        frame_counter_ff <= 8'd0;
    end else if (prepare_synccode_type) begin
        case (synccode_type_ff)
        SYNCCODE_TYPE_B:
            synccode_type_ff <= SYNCCODE_TYPE_W;
        SYNCCODE_TYPE_W:
            synccode_type_ff <= (frame_counter_ff == 8'd191) ? SYNCCODE_TYPE_B : SYNCCODE_TYPE_M;
        SYNCCODE_TYPE_M:
            synccode_type_ff <= SYNCCODE_TYPE_W;
        endcase

        if (frame_counter_ff <= 8'd191)
            frame_counter_ff <= 8'd0;
        else
            frame_counter_ff <= frame_counter_ff + 1;
    end
end
assign pop_ch = (synccode_type_ff == SYNCCODE_TYPE_W) ? 1'b1 : 1'b0;

reg [7:0] synccode_shiftreg;
always @(posedge clk) begin
    if (prepare_synccode) begin
        case (synccode_type_ff)
        SYNCCODE_TYPE_B:
            synccode_shiftreg <= phase ? SYNCCODE_B0 : SYNCCODE_B1;
        SYNCCODE_TYPE_W:
            synccode_shiftreg <= phase ? SYNCCODE_W0 : SYNCCODE_W1;
        SYNCCODE_TYPE_M:
            synccode_shiftreg <= phase ? SYNCCODE_M0 : SYNCCODE_M1;
        endcase
    end else
        synccode_shiftreg <= {synccode_shiftreg[6:0], 1'b0};
end

// FIXME
wire us = 1'b0;
wire cs = 1'b0;
wire parity = 1'b0;

reg [27:0] subframe_shiftreg;
always @(posedge clk) begin
    if (prepare_subframe) begin
        subframe_shiftreg <= {data_latch[23:0], 1'b1, us, cs, parity};
    end else if (halfbit)
        subframe_shiftreg <= {subframe_shiftreg[26:0], 1'b0};
end

wire subframe_bit = subframe_shiftreg[27];

function bmc_enc(
    input phase,
    input subframe_bit,
    input halfbit);
reg [1:0] encoded;
begin
    case({phase, subframe_bit})
        2'b0_0: 
            encoded = 2'b11;
        2'b0_1:
            encoded = 2'b10;
        2'b1_0:
            encoded = 2'b00;
        2'b1_1:
            encoded = 2'b01;
    endcase
    bmc_enc = encoded[halfbit];
end
endfunction

wire encoded_subframe = bmc_enc(phase, subframe_bit, halfbit_ff);

wire spdif_tbo = send_synccode ? synccode_shiftreg[7] : encoded_subframe;

reg phase_ff;
always @(posedge clk) begin
    if (rst)
        phase_ff <= 0;
    if (halfbit)
        phase_ff <= subframe_bit ^ phase_ff;
end
assign phase = phase_ff;

reg [1:0] spdif_out_ff;
always @(posedge clk)
    spdif_out_ff<= spdif_tbo;
assign spdif_o = spdif_out_ff;

assign pop_o = prepare_subframe ? {pop_ch, ~pop_ch} : 2'b0;

endmodule
