module decimator
#(
    parameter DATA_WIDTH = 24
)(
    input wire clk,
    input wire rst,

    input wire [2:0] factor,

    input wire [(DATA_WIDTH-1):0] data_i,
    input wire ack_i,

    output wire [(DATA_WIDTH-1):0] data_o,
    input wire ack_o);

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
