module async_fifo (
    // write if
    input wclk,
    input wrst,
    input [(DATA_WIDTH-1):0] data_i,
    input ack_i,
    output full_o,

    // read if
    input rclk,
    input rrst,
    output [(DATA_WIDTH-1):0] data_o,
    input pop_i,
    output empty_o);

reg [(DATA_WIDTH-1):0] mem [3:0];

// write if
reg [1:0] waddr_ff;
always @(posedge wclk) begin
    if (wrst) begin
        waddr_ff <= 2'b00;
    end else if (ack_i) begin
        mem[waddr_ff] <= data_i;
        waddr_ff <= waddr_ff + 2'b01;
    end
end

// read if
assign data_o = mem[raddr_ff];

reg [1:0] raddr_ff;
always @(posedge rclk) begin
    if (rrst) begin
        raddr_ff <= 2'b00;
    end else if (pop_i) begin
        raddr_ff <= raddr_ff + 2'b01;
    end
end

function [1:0] gray_enc(
    input [1:0] in);
    case (in)
        2'b00: gray_enc = 2'b00;
        2'b01: gray_enc = 2'b01;
        2'b10: gray_enc = 2'b11;
        2'b11: gray_enc = 2'b10;
    endcase
endfunction

function [1:0] gray_dec(
    input [1:0] in);
    case (in)
        2'b00: gray_dec = 2'b00;
        2'b01: gray_dec = 2'b01;
        2'b11: gray_dec = 2'b10;
        2'b10: gray_dec = 2'b11;
    endcase
endfunction

// empty notify
wire waddr_gray = gray_enc(waddr_ff);
reg [5:0] waddr_gray_delay_ff;
always @(posedge rclk)
    waddr_gray_delay_ff <= {waddr_gray, waddr_gray_delay_ff[5:2]};
wire waddr_delayed = gray_dec(waddr_gray_delay_ff[1:0]);

reg empty_ff;
always @(posedge rclk)
    empty_ff <= (waddr_delayed == raddr_ff);
assign empty_o = empty_ff;

// full notify
wire raddr_gray = gray_enc(raddr_ff);
reg [5:0] raddr_gray_delay_ff;
always @(posedge wclk)
    raddr_gray_delay_ff <= {raddr_gray, raddr_gray_delay_ff[5:2]};
wire raddr_delayed = gray_dec(raddr_gray_delay_ff[1:0]);

reg full_ff;
always @(posedge rclk)
    full_ff <= (raddr_delayed == waddr_ff);
assign full_o = full_ff;

endmodule
