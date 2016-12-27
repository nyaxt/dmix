module pop_latch(
    input wire clk,
    input wire rst,

    input wire pop_i,
    input wire ack_pop_i, // read enable
    output wire pop_latched_o);

reg latch;
always @(posedge clk) begin
    if (rst)
        latch <= 0;
    else
        latch <= pop_i | (~ack_pop_i & latch);
end
assign pop_latched_o = latch;

endmodule
