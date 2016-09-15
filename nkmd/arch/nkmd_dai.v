module nkmd_dai_rx(
    input clk,
    input rst,

    // dmix interface
    input [23:0] rx_data_i,
    input rx_ack_i,

    // To nkmm R bus
    input [31:0] data_i,
    output [31:0] data_o,
    input [31:0] addr_i,
    input we_i);

reg [5:0] lastw_ff;
always @(posedge clk) begin
    if (rst)
        lastw_ff <= 0;
    else if (rx_ack_i)
        lastw_ff <= lastw_ff + 1;
end

reg [23:0] ringbuf [63:0];
always @(posedge clk) begin
    if (rx_ack_i)
        ringbuf[lastw_ff] <= rx_data_i;
end

reg [5:0] unread_ff;
reg [5:0] shift_ff;
wire should_shift;
assign should_shift = we_i && addr_i[15:12] == 4'hd && addr_i[7:0] == 8'h00;
always @(posedge clk) begin
    if (rst) begin
        unread_ff <= 0;
        shift_ff <= 0;
    end else if (should_shift && !rx_ack_i) begin
        unread_ff <= unread_ff - 1;
        shift_ff <= shift_ff + 1;
    end else if (!should_shift && rx_ack_i) begin
        unread_ff <= unread_ff + 1;
    end else if (should_shift && rx_ack_i) begin
        shift_ff <= shift_ff + 1;
    end
end

// RAM interface
reg [31:0] data_o_ff;
assign data_o = data_o_ff;

// {4'hd, ch[3:0], 8'h00} => rx ringbuf unread
// {4'hf, ch[3:0], offset} => rx ringbuf
// FIXME: add ch check

wire [5:0] offset_i;
assign offset_i = addr_i[5:0];

always @(posedge clk) begin
    if (addr_i[15:12] == 4'hf)
        data_o_ff <= ringbuf[shift_ff + offset_i];
    else if (addr_i[15:12] == 4'hd && addr_i[7:0] == 8'h00)
        data_o_ff <= unread_ff;
    else
        data_o_ff <= 0;
end

endmodule

/*
module nkmd_dai_tx(
    input clk,
    input rst,

    // dmix interface
    output [23:0] tx_data_o,
    input tx_pop_i,
    output tx_ack_o,

    // To nkmm R bus
    input [31:0] data_i,
    output [31:0] data_o,
    input [31:0] addr_i,
    input we_i);

reg [5:0] lastr_ff;
always @(posedge clk) begin
    if (rst)
        lastr_ff <= 0;
    else if (rx_pop_i)
        lastr_ff <= lastr_ff + 1;
end

reg [23:0] ringbuf [63:0];
always @(posedge clk) begin
    if (rx_ack_i)
        ringbuf[lastr_ff] <= rx_data_i;
end

endmodule
*/
