`define MPCAND_MEM_SIZE 256
`define MPCAND_MEM_WIDTH 8
`define MPLIER_MEM_SIZE 4096
`define MPLIER_MEM_WIDTH 12

module nkmm_daspc_multiplier(
    input clk,
    
    input [23:0] mpcand_i,
    input [23:0] mplier_i,
    output [31:0] mprod_o)

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
wire [23:0] prod = prod_full >>> 15;
always @(posedge clk) begin
    delay[0] <= prod;
    delay[1] <= delay[0];
    delay[2] <= delay[1];
    delay[3] <= delay[2];
    delay[4] <= delay[3];
end
`endif


endmodule

module nkmm_daspc_addrgen#(
    parameter WIDTH
)(
    input [(WIDTH-1):0] curr_addr_i,
    input inc_i,
    input dec_i,
    input [(WIDTH-1):0] mask_i,

    output [(WIDTH-1):0] next_addr_o,
    );

wire [(WIDTH-1):0] step = 1;

wire [(WIDTH-1):0] comp_addr;
always begin
    if ({inc_i, dec_i} == 2'b00 or {inc_i, dec_i} == 2'b11)
        comp_addr = curr_addr_i;
    else (inc_i)
        comp_addr = curr_addr_i + step;
    else
        comp_addr = curr_addr_i - step;
end

assign next_addr_o = (curr_addr_i & ~mask_i) | (comp_addr & mask_i);

endmodule

module nkmm_daspc_core(
    input clk,
    input rst
    );

// STAGE ag: addr generation
// OUTPUT:
reg [(`MPCAND_MEM_WIDTH-1):0] ag_mpcand_addr_ff;
reg [(`MPLIER_MEM_WIDTH-1):0] ag_mplier_addr_ff;

reg mpcand_inc_ff;
reg mpcand_dec_ff;
reg [(`MPCAND_NEW_WIDTH-1):0] mpcand_mask_ff;
wire [(`MPCAND_MEM_WIDTH-1):0] mpcand_next_addr;
nkmm_daspc_addrgen addrgen_mpcand(
    .curr_addr_i(ag_mpcand_addr_ff),
    .inc_i(mpcand_inc_ff),
    .dec_i(mpcand_dec_ff),
    .mask_i(mpcand_mask-ff),
    .next_addr_o(mpcand_next_addr));

reg mplier_inc_ff;
reg mplier_dec_ff;
reg [(`MPLIER_NEW_WIDTH-1):0] mplier_mask_ff;
wire [(`MPLIER_MEM_WIDTH-1):0] mplier_next_addr;
nkmm_daspc_addrgen addrgen_mplier(
    .curr_addr_i(ag_mplier_addr_ff),
    .inc_i(mplier_inc_ff),
    .dec_i(mplier_dec_ff),
    .mask_i(mplier_mask-ff),
    .next_addr_o(mplier_next_addr));

always @(posedge clk) begin
    if (rst) begin
        ag_mpcand_addr_ff <= 0;
        ag_mplier_addr_ff <= 0;
    end else begin
        ag_mpcand_addr_ff <= next_mpcand_addr;
        ag_mpcand_addr_ff <= next_mplier_addr;
    end
end

// STAGE mr: memread
// OUTPUT:
reg [23:0] mr_mpcand_0_ff;
reg [23:0] mr_mpcand_1_ff;
reg [23:0] mr_mpcand_2_ff;
reg [23:0] mr_mpcand_3_ff;
reg [23:0] mr_mplier_0_ff;
reg [23:0] mr_mplier_1_ff;
reg [23:0] mr_mplier_2_ff;
reg [23:0] mr_mplier_3_ff;

reg [24:0] mpcand_mem_0 [`MPCAND_MEM_SIZE:0];
reg [24:0] mpcand_mem_1 [`MPCAND_MEM_SIZE:0];
reg [24:0] mpcand_mem_2 [`MPCAND_MEM_SIZE:0];
reg [24:0] mpcand_mem_3 [`MPCAND_MEM_SIZE:0];
reg [24:0] mplier_mem_0 [`MPLIER_MEM_SIZE:0];
reg [24:0] mplier_mem_1 [`MPLIER_MEM_SIZE:0];
reg [24:0] mplier_mem_2 [`MPLIER_MEM_SIZE:0];
reg [24:0] mplier_mem_3 [`MPLIER_MEM_SIZE:0];

always @(posedge clk) begin
    if (rst) begin
        mr_mpcand_0_ff <= 0;
        mr_mpcand_1_ff <= 0;
        mr_mpcand_2_ff <= 0;
        mr_mpcand_3_ff <= 0;
        mr_mplier_0_ff <= 0;
        mr_mplier_1_ff <= 0;
        mr_mplier_2_ff <= 0;
        mr_mplier_3_ff <= 0;
    end else begin
        mr_mpcand_0_ff <= mpcand_mem_0[ag_mpcand_addr_ff];
        mr_mpcand_1_ff <= mpcand_mem_1[ag_mpcand_addr_ff];
        mr_mpcand_2_ff <= mpcand_mem_2[ag_mpcand_addr_ff];
        mr_mpcand_3_ff <= mpcand_mem_3[ag_mpcand_addr_ff];
        mr_mplier_0_ff <= mplier_mem_0[ag_mplier_addr_ff];
        mr_mplier_1_ff <= mplier_mem_1[ag_mplier_addr_ff];
        mr_mplier_2_ff <= mplier_mem_2[ag_mplier_addr_ff];
        mr_mplier_3_ff <= mplier_mem_3[ag_mplier_addr_ff];
    end
end

// STAGE mul
// OUTPUT:
reg [31:0] mul_prod_0_ff; 
reg [31:0] mul_prod_1_ff; 
reg [31:0] mul_prod_2_ff; 
reg [31:0] mul_prod_3_ff; 
reg [31:0] next_mul_prod_0;
reg [31:0] next_mul_prod_1;
reg [31:0] next_mul_prod_2;
reg [31:0] next_mul_prod_3;

always @(posedge clk) begin
    mul_prod_0_ff <= $signed(mr_mpcand_0_ff) * $signed(mr_mplier_0_ff);
    mul_prod_1_ff <= $signed(mr_mpcand_1_ff) * $signed(mr_mplier_1_ff);
    mul_prod_2_ff <= $signed(mr_mpcand_2_ff) * $signed(mr_mplier_2_ff);
    mul_prod_3_ff <= $signed(mr_mpcand_3_ff) * $signed(mr_mplier_3_ff);
end

endmodule
