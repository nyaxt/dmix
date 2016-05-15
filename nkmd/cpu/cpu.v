module nkmd_cpu_if(
    input clk,
    input rst,

    input [31:0] p_data_i,
    output [31:0] p_addr_o,

    input [31:0] next_pc_i,
    output [31:0] inst_o);

assign p_addr_o = next_pc_i;
assign p_data_i = inst_o;

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

function [`DCD_WIDTH-1:0] nkmm_cpu_dcd_func(
    input [31:0] inst_i);

assign nkmm_cpu_dcd_func[`DCD_RSSEL+`DCD_REGSEL_W-1:`DCD_RSSEL] = inst_i[20:17];
assign nkmm_cpu_dcd_func[`DCD_RTSEL+`DCD_REGSEL_W-1:`DCD_RTSEL] = inst_i[11:8];
assign nkmm_cpu_dcd_func[`DCD_RDSEL+`DCD_REGSEL_W-1:`DCD_RDSEL] = inst_i[27:4];
assign nkmm_cpu_dcd_func[`DCD_ALUSEL+`DCD_ALUSEL_W-1:`DCD_ALUSEL] = inst_i[23:21];
assign nkmm_cpu_dcd_func[`DCD_IMM+`DCD_IMM_W-1:`DCD_IMM] = {17{inst_i[15]}, inst_i[14:0]};
assign nkmm_cpu_dcd_func[`DCD_JMPREL+`DCD_JMPREL_W-1:`DCD_JMPREL] = {25{inst_i[15]}, inst_i[7:0]};

endfunction

module nkmd_cpu_dcd(
    input clk,
    input rst,

    input [31:0] inst_i,

    
    );

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
wire [31:0] dcd_mem;


// WB -> IF
wire [31:0] wb_if_next_pc;

// IF: Instruction Fetch
nkmd_cpu_if nkmd_cpu_if(
    .clk(clk), .rst(rst),
    .p_data_i(p_data_i), .p_addr_o(p_addr_o)
    .next_pc_i(wb_if_next_pc),
    .inst_o(if_dcd_inst));

// DCD: instruction DeCoDe
nkmd_cpu_dcd nkmd_cpu_dcd(
    .clk(clk), .rst(rst),
    .inst_i(if_dcd_inst),
    );

endmodule
