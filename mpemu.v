// simulates multiplier ip core provided by xillinx
// latency 4

module mpemu(
    input clk,
    
    input signed [23:0] mpcand_i,
    input signed [15:0] mplier_i,
    output signed [23:0] mprod_o);

reg signed [23:0] delay[3:0];
assign mprod_o = delay[3];

wire [23:0] prod = (mpcand_i * mplier_i) >>> 15;
always @(posedge clk) begin
    delay[0] <= prod;
    delay[1] <= delay[0];
    delay[2] <= delay[1];
    delay[3] <= delay[2];
end

endmodule
