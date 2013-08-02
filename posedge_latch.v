module posedge_latch(
    input clk,

    input wpulse_i,
    output ack_o);

reg wpulse_handled_ff;
// initial wpulse_handled_ff = 1;
reg ack_ff;
// initial ack_ff = 1;
assign ack_o = ack_ff;
always @(posedge clk) begin
    ack_ff <= 0;

    if(wpulse_i && !wpulse_handled_ff) begin
        wpulse_handled_ff <= 1;
        ack_ff <= 1; 
    end else if(!wpulse_i)
        wpulse_handled_ff <= 0;
end

endmodule
