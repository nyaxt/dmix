module decimator
#(
    parameter DATA_WIDTH = 24
)(
    input clk,
    input rst,

    input [2:0] factor,

    input [(DATA_WIDTH-1):0] data_i,
    input ack_i,

    output [(DATA_WIDTH-1):0] data_o,
    input ack_o);

reg [(DATA_WIDTH-1):0] latch;
always @(posedge clk) begin
    if (ack_i)
        latch <= data_i;
end
assign data_o = latch;

reg [2:0] counter;
wire match = counter == factor;
always @(posedge clk) begin
    if (rst)
        counter <= 0;
    else if (ack_i) begin
        if (match)
            counter <= 0;
        else
            counter <= counter + 1;
    end
end

reg ack_ff;
always @(posedge clk)
    ack_ff <= ack_i & match;
assign ack_o = ack_ff;

endmodule
