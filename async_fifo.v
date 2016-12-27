module async_fifo
#(
    parameter DATA_WIDTH = 8    
)(
    // write if
    input wire wclk,
    input wire wrst,
    input wire [(DATA_WIDTH-1):0] data_i,
    input wire ack_i,
    output wire full_o, // use with caution. may be buggy

    // read if
    input wire rclk,
    input wire rrst,
    output wire [(DATA_WIDTH-1):0] data_o,
    input wire pop_i,
    output wire empty_o);

reg [(DATA_WIDTH-1):0] mem [3:0];

// write if
reg [2:0] waddr_ff;
wire [2:0] waddr_next = waddr_ff + (ack_i ? 3'b001 : 3'b000);
always @(posedge wclk) begin
    if (wrst)
        waddr_ff <= 3'b000;
    else
        waddr_ff <= waddr_next;
end

always @(posedge wclk)
    if (ack_i)
        mem[waddr_ff[1:0]] <= data_i;

// read if
reg [2:0] raddr_ff;
wire [2:0] raddr_next = raddr_ff + (pop_i ? 3'b001 : 3'b000);
always @(posedge rclk) begin
    if (rrst)
        raddr_ff <= 3'b000;
    else
        raddr_ff <= raddr_next;
end

assign data_o = mem[raddr_ff[1:0]];

function [2:0] gray_enc(
    input [2:0] in);
    case (in)
    3'b000: gray_enc = 3'b000;
    3'b001: gray_enc = 3'b001;
    3'b010: gray_enc = 3'b011;
    3'b011: gray_enc = 3'b010;
    3'b100: gray_enc = 3'b110;
    3'b101: gray_enc = 3'b111;
    3'b110: gray_enc = 3'b101;
    3'b111: gray_enc = 3'b100;
    endcase
endfunction

function [2:0] gray_dec(
    input  [2:0] in);
    case (in)
    3'b000: gray_dec = 3'b000;
    3'b001: gray_dec = 3'b001;
    3'b011: gray_dec = 3'b010;
    3'b010: gray_dec = 3'b011;
    3'b110: gray_dec = 3'b100;
    3'b111: gray_dec = 3'b101;
    3'b101: gray_dec = 3'b110;
    3'b100: gray_dec = 3'b111;
    endcase
endfunction

// empty notify
wire [2:0] waddr_gray = gray_enc(waddr_ff);
reg [8:0] waddr_gray_delay_ff;
always @(posedge rclk) begin
    if (rrst)
        waddr_gray_delay_ff <= 9'b0;
    else
        waddr_gray_delay_ff <= {waddr_gray, waddr_gray_delay_ff[8:3]};
end
wire [2:0] waddr_delayed = gray_dec(waddr_gray_delay_ff[2:0]);

assign empty_o = (waddr_delayed[1:0] == raddr_next[1:0]) && (waddr_delayed[2] == raddr_next[2]);

// full notify
wire [2:0] raddr_gray = gray_enc(raddr_ff);
reg [8:0] raddr_gray_delay_ff;
always @(posedge wclk) begin
    if (wrst)
        raddr_gray_delay_ff <= 9'b0;
    else
        raddr_gray_delay_ff <= {raddr_gray, raddr_gray_delay_ff[8:3]};
end
wire [2:0] raddr_delayed = gray_dec(raddr_gray_delay_ff[2:0]);

assign full_o = (raddr_delayed[1:0] == waddr_next[1:0]) && (raddr_delayed[2] != waddr_next[2]);
reg last_full_ff;
always @(posedge wclk)
    last_full_ff <= full_o;

endmodule
