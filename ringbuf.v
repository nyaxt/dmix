`undef DEBUG

module ringbuf
#(
    parameter LEN = 16,
    parameter LEN_LOG2 = 4,
    parameter RITER_START = LEN/2
)(
    input clk,
    input rst,

    input [23:0] data_i,
    input we_i,
    
    input pop_i,
    input [(LEN_LOG2-1):0] offset_i,
    output [23:0] data_o);

reg [23:0] mem [(LEN-1):0];

// WRITE
reg [(LEN_LOG2-1):0] witer;
always @(posedge clk) begin
    if(rst) begin
        witer <= 0;
    end else begin
        if(we_i) begin
        `ifdef DEBUG
            $display("ringbuf: write addr: %d/%d data: %h", witer, LEN, data_i);
        `endif
            mem[witer] <= data_i;
            witer <= witer + 1;
        end
    end
end

// READ
reg [(LEN_LOG2-1):0] riter;
wire [(LEN_LOG2-1):0] raddr = riter - offset_i;

reg [23:0] data_ff;
assign data_o = data_ff;

always @(posedge clk) begin
    if(rst)
        riter <= RITER_START;
    else begin
        if(pop_i)
            riter <= riter + 1;
            
        data_ff <= mem[raddr];
    end
end

endmodule
