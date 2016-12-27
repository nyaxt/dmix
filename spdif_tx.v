module spdif_tx(
    input wire clk, // 24.576Mhz
    input wire rst,

    input wire [1:0] ack_i,
	input [47:0] data_i,
    output wire [1:0] pop_o,

    input wire [191:0] udata_i,
    input wire [191:0] cdata_i,

    output wire spdif_o
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
wire send_parity = subframe_pos_counter_ff == 5'd31;
wire prepare_subframe = halfbit & (subframe_pos_counter_ff == 5'd3);
wire prepare_synccode_type = ~halfbit & (subframe_pos_counter_ff == 5'd31);
wire prepare_synccode = halfbit & (subframe_pos_counter_ff == 5'd31);

wire prev_subframe_end;

reg [2:0] synccode_type_ff;
parameter SYNCCODE_TYPE_B = 0;
parameter SYNCCODE_TYPE_W = 1;
parameter SYNCCODE_TYPE_M = 2;
reg [7:0] frame_counter_ff;
wire end_of_frame = frame_counter_ff == 8'd191;
always @(posedge clk) begin
    if (rst) begin
        synccode_type_ff <= SYNCCODE_TYPE_B;
        frame_counter_ff <= 8'd191;
    end else if (prepare_synccode_type) begin
        case (synccode_type_ff)
        SYNCCODE_TYPE_B:
            synccode_type_ff <= SYNCCODE_TYPE_W;
        SYNCCODE_TYPE_W:
            synccode_type_ff <= end_of_frame ? SYNCCODE_TYPE_B : SYNCCODE_TYPE_M;
        SYNCCODE_TYPE_M:
            synccode_type_ff <= SYNCCODE_TYPE_W;
        endcase

        if (end_of_frame)
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
            synccode_shiftreg <= prev_subframe_end ? SYNCCODE_B0 : SYNCCODE_B1;
        SYNCCODE_TYPE_W:
            synccode_shiftreg <= prev_subframe_end ? SYNCCODE_W0 : SYNCCODE_W1;
        SYNCCODE_TYPE_M:
            synccode_shiftreg <= prev_subframe_end ? SYNCCODE_M0 : SYNCCODE_M1;
        endcase
    end else
        synccode_shiftreg <= {synccode_shiftreg[6:0], 1'b0};
end

// FIXME
wire [23:0] data_active = pop_ch ? data_latch[47:24] : data_latch[23:0];

// {User,Control} data
reg [191:0] udata_shiftreg;
reg [191:0] cdata_shiftreg;
always @(posedge clk) begin
    if (end_of_frame)
        udata_shiftreg <= udata_i;
    else if (prepare_subframe)
        udata_shiftreg <= {udata_shiftreg[190:0], 1'b0};

    if (end_of_frame)
        cdata_shiftreg <= cdata_i;
    else if (prepare_subframe)
        cdata_shiftreg <= {cdata_shiftreg[190:0], 1'b0};
end

reg [26:0] subframe_shiftreg;
always @(posedge clk) begin
    if (prepare_subframe) begin
        subframe_shiftreg <= {data_active, 1'b1, udata_shiftreg[191], cdata_shiftreg[191]};
    end else if (halfbit)
        subframe_shiftreg <= {subframe_shiftreg[25:0], 1'b0};
end

wire subframe_bit = subframe_shiftreg[26];

reg parity_ff;
always @(posedge clk) begin
    if (prepare_subframe)
        parity_ff <= 0;
    else if (halfbit)
        parity_ff <= parity_ff ^ subframe_bit;
end
wire parity = parity_ff;

wire subframe_or_parity_bit = send_parity ? parity : subframe_shiftreg[26];

reg encoded_subframe_ff;
always @(posedge clk) begin
    if (rst)
        encoded_subframe_ff <= 0;
    else
        encoded_subframe_ff <= (subframe_or_parity_bit | halfbit) ^ encoded_subframe_ff;
end
wire spdif_tbo = send_synccode ? synccode_shiftreg[7] : encoded_subframe_ff;
assign prev_subframe_end = encoded_subframe_ff;

reg spdif_out_ff;
always @(posedge clk)
    spdif_out_ff <= spdif_tbo;
assign spdif_o = spdif_out_ff;

assign pop_o = prepare_subframe ? {pop_ch, ~pop_ch} : 2'b0;

endmodule
