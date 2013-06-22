module posedge_latch(
    input clk,

    input wpulse_i,
    output pop_o);

reg wpulse_handled_ff;
reg pop_ff;
assign pop_o = pop_ff;
always @(negedge clk) begin
    pop_ff <= 0;

    if(wpulse_i && !wpulse_handled_ff) begin
        wpulse_handled_ff <= 1;
        pop_ff <= 1; 
    end else if(!wpulse_i)
        wpulse_handled_ff <= 0;
end


endmodule
