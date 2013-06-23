// simulates multiplier ip core provided by xillinx
// latency 4
//`define USE_IP

module mpemu(
    input clk,
    
    input signed [23:0] mpcand_i,
    input signed [15:0] mplier_i,
    output signed [23:0] mprod_o);

`ifdef USE_IP
mp mp(
  .clk(clk),
  .a(mpcand_i),
  .b(mplier_i),
  .p(mprod_o));
`else
reg signed [23:0] delay[4:0];
assign mprod_o = delay[4];

wire [39:0] prod_full = mpcand_i * mplier_i;
wire [23:0] prod = prod_full >>> 16;
always @(posedge clk) begin
    delay[0] <= prod;
    delay[1] <= delay[0];
    delay[2] <= delay[1];
    delay[3] <= delay[2];
    delay[4] <= delay[3];
end
`endif

endmodule
