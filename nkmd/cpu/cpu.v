`default_nettype none

module nkmd_cpu_if(
    input clk,
    input rst,

    input [31:0] p_data_i,
    output [31:0] p_addr_o,

    input [31:0] next_pc_i,
    output [31:0] inst_o);

assign p_addr_o = next_pc_i;
assign inst_o = p_data_i ;

endmodule

`define DCD_REGSEL_W 4
`define DCD_RSSEL 0
`define DCD_RTSEL (`DCD_RSSEL + `DCD_REGSEL_W)
`define DCD_RDSEL (`DCD_RTSEL + `DCD_REGSEL_W)

`define DCD_ALUSEL_W 3
`define DCD_ALUSEL (`DCD_RDSEL + `DCD_REGSEL_W)

`define DCD_IMM_W 32
`define DCD_IMM (`DCD_ALUSEL + `DCD_ALUSEL_W)

`define DCD_JMPREL_W 32 
`define DCD_JMPREL (`DCD_IMM + `DCD_IMM_W)

`define DCD_WIDTH (`DCD_JMPREL + `DCD_JMPREL_W)

module nkmd_cpu_dcd(
    input clk,
    input rst,

    input [31:0] inst_i,

    output [`DCD_REGSEL_W-1:0] rssel_o,    
    output [`DCD_REGSEL_W-1:0] rtsel_o,    
    output [`DCD_REGSEL_W-1:0] rdsel_o,    
    output [`DCD_ALUSEL_W-1:0] alusel_o,
    output [`DCD_IMM_W-1:0] imm_o,
    output [`DCD_JMPREL_W-1:0] jmprel_o
    );

reg [`DCD_REGSEL_W-1:0] rssel_ff;
reg [`DCD_REGSEL_W-1:0] rtsel_ff;
reg [`DCD_REGSEL_W-1:0] rdsel_ff;
reg [`DCD_ALUSEL_W-1:0] alusel_ff;
reg [`DCD_IMM_W-1:0] imm_ff;
reg [`DCD_JMPREL_W-1:0] jmprel_ff;

function [`DCD_WIDTH-1:0] nkmd_cpu_dcd_func(
    input [31:0] inst_i);
begin
    nkmd_cpu_dcd_func[`DCD_RSSEL+`DCD_REGSEL_W-1:`DCD_RSSEL] = inst_i[20:17];
    nkmd_cpu_dcd_func[`DCD_RTSEL+`DCD_REGSEL_W-1:`DCD_RTSEL] = inst_i[11:8];
    nkmd_cpu_dcd_func[`DCD_RDSEL+`DCD_REGSEL_W-1:`DCD_RDSEL] = inst_i[27:4];
    nkmd_cpu_dcd_func[`DCD_ALUSEL+`DCD_ALUSEL_W-1:`DCD_ALUSEL] = inst_i[23:21];
    nkmd_cpu_dcd_func[`DCD_IMM+`DCD_IMM_W-1:`DCD_IMM] = {{17{inst_i[15]}}, inst_i[14:0]};
    nkmd_cpu_dcd_func[`DCD_JMPREL+`DCD_JMPREL_W-1:`DCD_JMPREL] = {{25{inst_i[15]}}, inst_i[7:0]};
end
endfunction

always @(posedge clk) begin
    {rssel_ff, rtsel_ff, rdsel_ff, alusel_ff, imm_ff, jmprel_ff} <= nkmd_cpu_dcd_func(inst_i);
end

assign rssel_o = rssel_ff;
assign rtsel_o = rtsel_ff;
assign rdsel_o = rdsel_ff;
assign alusel_o = alusel_ff;
assign imm_o = imm_ff;
assign jmprel_o = jmprel_ff;

endmodule

module nkmd_cpu_regfile(
    input clk,
    input rst,

    input [`DCD_REGSEL_W-1:0] dcd_rssel_i,
    input [`DCD_REGSEL_W-1:0] dcd_rtsel_i,
    output [31:0] mem_rsval_o,
    output [31:0] mem_rtval_o);

reg [31:0] a_ff;
reg [31:0] b_ff;
reg [31:0] c_ff;
reg [31:0] d_ff;
reg [31:0] e_ff;
reg [31:0] f_ff;
reg [31:0] g_ff;
reg [31:0] h_ff;
reg [31:0] i_ff;
reg [31:0] j_ff;
reg [31:0] sl_ff;
reg [31:0] sh_ff;
reg [31:0] n_ff;

function [31:0] nkmd_cpu_regfile_sel(
    input [`DCD_REGSEL_W-1:0] sel,
    input [31:0] a,
    input [31:0] b,
    input [31:0] c,
    input [31:0] d,
    input [31:0] e,
    input [31:0] f,
    input [31:0] g,
    input [31:0] h,
    input [31:0] i,
    input [31:0] j,
    input [31:0] sl,
    input [31:0] sh,
    input [31:0] n);
begin
    case (sel)
    4'h0: nkmd_cpu_regfile_sel = 0;
    4'h1: nkmd_cpu_regfile_sel = a;
    4'h2: nkmd_cpu_regfile_sel = b;
    4'h3: nkmd_cpu_regfile_sel = c;
    4'h4: nkmd_cpu_regfile_sel = d;
    4'h5: nkmd_cpu_regfile_sel = e;
    4'h6: nkmd_cpu_regfile_sel = f;
    4'h7: nkmd_cpu_regfile_sel = g;
    4'h8: nkmd_cpu_regfile_sel = h;
    4'h9: nkmd_cpu_regfile_sel = i;
    4'ha: nkmd_cpu_regfile_sel = j;
    4'hb: nkmd_cpu_regfile_sel = 32'hXXXX; // ra: ret addr
    4'hc: nkmd_cpu_regfile_sel = sl;
    4'hd: nkmd_cpu_regfile_sel = sh;
    4'he: nkmd_cpu_regfile_sel = n;
    4'hf: nkmd_cpu_regfile_sel = 32'hXXXX; // pc: program counter
    endcase
end
endfunction

reg [31:0] mem_rsval_ff;
reg [31:0] mem_rtval_ff;
always @(posedge clk) begin
    mem_rsval_ff <= nkmd_cpu_regfile_sel(dcd_rssel_i,
        a_ff, b_ff, c_ff, d_ff, e_ff, f_ff, g_ff, h_ff, i_ff, j_ff, sl_ff, sh_ff, n_ff);
    mem_rtval_ff <= nkmd_cpu_regfile_sel(dcd_rtsel_i,
        a_ff, b_ff, c_ff, d_ff, e_ff, f_ff, g_ff, h_ff, i_ff, j_ff, sl_ff, sh_ff, n_ff);
end
assign mem_rsval_o = mem_rsval_ff;
assign mem_rtval_o = mem_rtval_ff;

always @(posedge clk) begin
    if (rst) begin
        a_ff <= 31'd0;
        b_ff <= 31'd0;
        c_ff <= 31'd0;
        d_ff <= 31'd0;
        e_ff <= 31'd0;
        f_ff <= 31'd0;
        g_ff <= 31'd0;
        h_ff <= 31'd0;
        i_ff <= 31'd0;
        j_ff <= 31'd0;
        sl_ff <= 31'd0;
        sh_ff <= 31'd0;
        n_ff <= 31'd0;
    end
end

endmodule

module nkmd_cpu_mem(
    input clk,
    input rst,

    // BUS R
    input [31:0] r_data_i,
    output [31:0] r_data_o,
    output [31:0] r_addr_o,
    output r_we_o,

    // BUS C: RAM2
    input [31:0] c_data_i,
    output [31:0] c_data_o,
    output [31:0] c_addr_o,
    output c_we_o

    // MEM stage
    input [31:0] mem_r_addr_i,
    input mem_r_read_en,
    output [31:0] mem_r_data_o,
    input [31:0] mem_c_addr_i,
    input mem_c_read_en,
    output [31:0] mem_c_data_o);

// FIXME: would need arbitration with WB stage in future
assign r_data_o = 32'b0; // FIXME
assign r_addr_o = mem_r_addr_i;
assign r_we_o = 1'b0;
assign mem_r_data_o = r_data_i;

assign c_data_o = 32'b0; // FIXME
assign c_addr_o = mem_c_addr_i;
assign c_we_o = 1'b0;
assign mem_c_data_o = c_data_i;

endmodule

module nkmd_cpu_ex(
    input clk,
    input rst,
    
    input [31:0] val_i,

    output [31:0] val_o,

    // to regfile
    output [`DCD_REGSEL_W-1:0] regex_sel_o,
    output regex_write_en,

    // to mem
    output [31:0] memex_addr_o,
    );

always @(posedge clk) begin
end

endmodule


module nkmd_cpu(
    input clk,
    input rst,

    // BUS R: RAM + MMAPIO
    input [31:0] r_data_i,
    output [31:0] r_data_o,
    output [31:0] r_addr_o,
    output r_we_o,

    // BUS P: PROGRAM
    input [31:0] p_data_i,
    output [31:0] p_addr_o,

    // BUS C: RAM2
    input [31:0] c_data_i,
    output [31:0] c_data_o,
    output [31:0] c_addr_o,
    output c_we_o);

// IF -> DCD: Instruction Fetch -> DeCoDe
wire [31:0] if_dcd_inst;

// DCD -> MEM
wire [`DCD_REGSEL_W-1:0] dcd_mem_rssel;
wire [`DCD_REGSEL_W-1:0] dcd_mem_rtsel;
wire [`DCD_REGSEL_W-1:0] dcd_mem_rdsel;
wire [`DCD_ALUSEL_W-1:0] dcd_mem_alusel;
wire [`DCD_IMM_W-1:0] dcd_mem_imm;
wire [`DCD_JMPREL_W-1:0] dcd_mem_jmprel;

// DCD -> RF
wire [`DCD_REGSEL_W-1:0] dcd_rf_rssel = dcd_mem_rssel;
wire [`DCD_REGSEL_W-1:0] dcd_rf_rtsel = dcd_mem_rtsel;

// RF -> MEM
wire [31:0] rf_mem_rsval;
wire [31:0] rf_mem_rtval;

// MEM -> EX
wire [31:0] mem_ex_rsval;
wire [31:0] mem_ex_rtval;
wire [`DCD_ALUSEL_W-1:0] mem_ex_alusel;

// WB -> IF
wire [31:0] wb_if_next_pc;

// IF: Instruction Fetch
nkmd_cpu_if nkmd_cpu_if(
    .clk(clk), .rst(rst),
    .p_data_i(p_data_i),
    .p_addr_o(p_addr_o),
    .next_pc_i(wb_if_next_pc),
    .inst_o(if_dcd_inst));

// DCD: instruction DeCoDe
nkmd_cpu_dcd nkmd_cpu_dcd(
    .clk(clk), .rst(rst),
    .inst_i(if_dcd_inst),
    .rssel_o(dcd_mem_rssel),
    .rtsel_o(dcd_mem_rtsel),
    .rdsel_o(dcd_mem_rdsel),
    .alusel_o(dcd_mem_alusel),
    .imm_o(dcd_mem_imm),
    .jmprel_o(dcd_mem_jmprel));

// RF: Register File
nkmd_cpu_regfile nkmd_cpu_regfile(
    .clk(clk), .rst(rst),
    .dcd_rssel_i(dcd_rf_rssel),
    .dcd_rtsel_i(dcd_rf_rtsel),
    .mem_rsval_o(rf_mem_rsval),
    .mem_rtval_o(rf_mem_rtval));

// MEM: Memory fetch
nkmd_cpu_mem nkmd_cpu_mem(
    .clk(clk), .rst(rst),

    .r_data_i(r_data_i),
    .r_data_o(r_data_o),
    .r_addr_o(r_addr_o),
    .r_we_o(r_we_o),
    .c_data_i(c_data_i),
    .c_data_o(c_data_o),
    .c_addr_o(c_addr_o),
    .c_we_o(c_we_o),

    .mem_r_addr_i(rf_mem_rsval),
    .mem_r_read_en(dcd_mem_r_read_en),
    .mem_r_data_o(mem_ex_sval),
    .mem_c_addr_i(rf_mem_rtval),
    .mem_c_read_en(dcd_mem_c_read_en),
    .mem_c_data_o(mem_ex_tval));

// EX: EXecute alu
nkmd_cpu_ex nkmd_cpu_ex(
    .clk(clk), .rst(rst),

    .rsval_i(mem_ex_rsval),
    .rtval_i(mem_ex_rtval),
    .alusel_i(mem_ex_alusel),

    .val_o(ex_wb_val));

endmodule
