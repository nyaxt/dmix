module ringbuf(
    input clk,
    input rst,

    input [23:0] data_i,
    input wpulse_i,
    
    input pop_i,
    input [3:0] offset_i,
    output [23:0] data_o);

reg [23:0] mem [15:0];

// WRITE
reg wpulse_handled_ff;
reg [3:0] witer;
always @(posedge clk) begin
    if(rst) begin
        witer <= 0;
    end else if(wpulse_i && !wpulse_handled_ff) begin
        wpulse_handled_ff <= 1;
        mem[witer] <= data_i;
        witer <= witer + 1;
    end else if(!wpulse_i)
        wpulse_handled_ff <= 0;
end

// READ
reg [3:0] riter;
wire [3:0] raddr = riter - offset_i;
assign data_o = mem[raddr];

always @(posedge clk) begin
    if(rst)
        riter <= 15;
    else if(pop_i)
        riter <= riter + 1; 
end

endmodule
