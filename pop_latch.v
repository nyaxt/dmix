module pop_latch(
    input clk,
    input rst,

    input pop_i,
    input ack_pop_i, // read enable
    output pop_latched_o);

reg latch;
always @(posedge clk) begin
    if (rst)
        latch <= 0;
    else
        latch <= pop_i | (~ack_pop_i & latch);
end
assign pop_latched_o = latch;

endmodule
