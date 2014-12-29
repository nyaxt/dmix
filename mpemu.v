// simulates multiplier ip core provided by xilinx
 `define USE_IP

module mpemu(
    input clk,
    
    input [23:0] mpcand_i,
    input [23:0] mplier_i,
    output [23:0] mprod_o);

`ifdef USE_IP
// A: signed 24 bit
// B: signed 24 bit
// P: Custom Output Width MSB=46 LSB=23
// Pipeline Stages 5
mp mp(
  .clk(clk),
  .a(mpcand_i),
  .b(mplier_i),
  .p(mprod_o));
`else
reg [23:0] delay_a[4:0];
reg [23:0] delay_b[4:0];

always @(posedge clk) begin
    delay_a[0] <= mpcand_i;
    delay_a[1] <= delay_a[0];
    delay_a[2] <= delay_a[1];
    delay_a[3] <= delay_a[2];
    delay_a[4] <= delay_a[3];

    delay_b[0] <= mplier_i;
    delay_b[1] <= delay_b[0];
    delay_b[2] <= delay_b[1];
    delay_b[3] <= delay_b[2];
    delay_b[4] <= delay_b[3];
end
wire [23:0] delayed_a = delay_a[4];
wire [23:0] delayed_b = delay_b[4];

wire [46:0] prod_full = $signed(delayed_a) * $signed(delayed_b);
assign mprod_o = $signed(prod_full) >>> 23;
`endif

endmodule

module mpemu_scale(
    input clk,
    
    input signed [23:0] mpcand_i,
    input [15:0] scale_i,
    output signed [23:0] mprod_o);

`ifdef USE_IP
mp_scale mp_scale(
  .clk(clk),
  .a(mpcand_i),
  .b(scale_i),
  .p(mprod_o));
`else
wire signed [16:0] mplier_i = scale_i;
reg signed [23:0] delay[4:0];
assign mprod_o = delay[4];

wire [39:0] prod_full = mpcand_i * mplier_i;
wire [23:0] prod = mpcand_i;
always @(posedge clk) begin
    delay[0] <= prod;
    delay[1] <= delay[0];
    delay[2] <= delay[1];
    delay[3] <= delay[2];
    delay[4] <= delay[3];
end
`endif

endmodule
