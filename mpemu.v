// simulates multiplier ip core provided by xilinx

module mpemu(
    input clk,
    
    input [23:0] mpcand_i,
    input [23:0] mplier_i,
    output [27:0] mprod_o);

`ifndef NO_IP
// A: signed 24 bit
// B: signed 24 bit
// P: Custom Output Width MSB=46 LSB=19
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
assign mprod_o = prod_full[46:19];
`endif

endmodule

module mpemu_scale(
    input clk,
    
    input [23:0] mpcand_i,
    input [31:0] scale_i,
    output [31:0] mprod_o);

`ifndef NO_IP
// A: signed 24 bit
// B: unsigned 32 bit
// P: Custom Output Width MSB=55 LSB=24
// Pipeline Stages 6

mp_scale mp_scale(
  .clk(clk),
  .a(mpcand_i),
  .b(scale_i),
  .p(mprod_o));
`else
reg [23:0] delay_a[6:0];
reg [31:0] delay_b[6:0];

always @(posedge clk) begin
    delay_a[0] <= mpcand_i;
    delay_a[1] <= delay_a[0];
    delay_a[2] <= delay_a[1];
    delay_a[3] <= delay_a[2];
    delay_a[4] <= delay_a[3];
    delay_a[5] <= delay_a[4];
    delay_a[6] <= delay_a[5];

    delay_b[0] <= scale_i;
    delay_b[1] <= delay_b[0];
    delay_b[2] <= delay_b[1];
    delay_b[3] <= delay_b[2];
    delay_b[4] <= delay_b[3];
    delay_b[5] <= delay_b[4];
    delay_b[6] <= delay_b[5];
end
wire [23:0] delayed_a = delay_a[5];
wire [31:0] delayed_b = delay_b[5];
wire signed [32:0] delayed_b_signed = {1'b0, delay_b[5]};

// for saturated delay
wire [23:0] delayed_a2 = delay_a[6];
wire [31:0] delayed_b2 = delay_b[6];

wire signed [55:0] prod_full = $signed(delayed_a) * $signed(delayed_b_signed);
wire mprod_o = prod_full[55:24];
`endif

endmodule
