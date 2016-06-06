`default_nettype none
`timescale 1ns / 1ps

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

`define DCD_TVAL_R_READ_EN (`DCD_JMPREL + `DCD_JMPREL_W)
`define DCD_SVAL_C_READ_EN (`DCD_TVAL_R_READ_EN + 1)
`define DCD_IMMEN (`DCD_SVAL_C_READ_EN + 1)
`define DCD_JMPEN (`DCD_IMMEN + 1)
`define DCD_REPN (`DCD_JMPEN + 1)

`define DCD_WIDTH (`DCD_REPN + 1)

`define OP_ADD 3'h0
`define OP_SUB 3'h1
`define OP_OR 3'h2
`define OP_AND 3'h3
`define OP_XOR 3'h4
`define OP_RESERVED 3'h5
`define OP_CLAMP 3'h6
`define OP_MUL 3'h7

module nkmd_cpu_if(
    input clk,
    input rst,

    input [31:0] p_data_i,
    output [31:0] p_addr_o,

    input seq_stop_inc_pc_i,
    input [31:0] jmp_pc_i,
    input jmp_pc_en_i,
    output [31:0] inst_o);

assign p_addr_o = pc_ff;
assign inst_o = p_data_i;

reg [31:0] pc_ff;
always @(posedge clk) begin
    if (rst) begin
        pc_ff <= 32'h0;
    end else begin
        if (jmp_pc_en_i)
            pc_ff <= jmp_pc_i;
        else if (!seq_stop_inc_pc_i)
            pc_ff <= pc_ff + 1;
    end
end

endmodule

module nkmd_cpu_dcd(
    input clk,
    input rst,

    input [31:0] inst_i,

    output [`DCD_REGSEL_W-1:0] rssel_o,    
    output [`DCD_REGSEL_W-1:0] rtsel_o,    
    output [`DCD_REGSEL_W-1:0] rdsel_o,    
    output [`DCD_ALUSEL_W-1:0] alusel_o,
    output [`DCD_IMM_W-1:0] imm_o,
    output [`DCD_JMPREL_W-1:0] jmprel_o,
    output tval_r_read_en_o,
    output sval_c_read_en_o,
    output imm_en_o,
    output jmp_en_o,
    output repn_o);

reg [`DCD_REGSEL_W-1:0] rssel_ff;
reg [`DCD_REGSEL_W-1:0] rtsel_ff;
reg [`DCD_REGSEL_W-1:0] rdsel_ff;
reg [`DCD_ALUSEL_W-1:0] alusel_ff;
reg [`DCD_IMM_W-1:0] imm_ff;
reg [`DCD_JMPREL_W-1:0] jmprel_ff;
reg tval_r_read_en_ff;
reg sval_c_read_en_ff;
reg imm_en_ff;
reg jmp_en_ff;
reg repn_ff;

function [`DCD_WIDTH-1:0] nkmd_cpu_dcd_func(
    input [31:0] inst_i);

reg jmpen;
reg memrw;
reg immen;

begin
    jmpen = inst_i[31];
    memrw = inst_i[30];
    immen = inst_i[16];

    nkmd_cpu_dcd_func[`DCD_RSSEL+`DCD_REGSEL_W-1:`DCD_RSSEL] = inst_i[20:17];
    nkmd_cpu_dcd_func[`DCD_RTSEL+`DCD_REGSEL_W-1:`DCD_RTSEL] = inst_i[11:8];
    nkmd_cpu_dcd_func[`DCD_RDSEL+`DCD_REGSEL_W-1:`DCD_RDSEL] = inst_i[27:24];
    nkmd_cpu_dcd_func[`DCD_ALUSEL+`DCD_ALUSEL_W-1:`DCD_ALUSEL] = inst_i[23:21];
    nkmd_cpu_dcd_func[`DCD_IMM+`DCD_IMM_W-1:`DCD_IMM] = {{17{inst_i[15]}}, inst_i[14:0]};
    nkmd_cpu_dcd_func[`DCD_JMPREL+`DCD_JMPREL_W-1:`DCD_JMPREL] = {{25{inst_i[15]}}, inst_i[7:0]};
    nkmd_cpu_dcd_func[`DCD_TVAL_R_READ_EN] = !jmpen && inst_i[28];
    nkmd_cpu_dcd_func[`DCD_SVAL_C_READ_EN] = !jmpen && !immen && inst_i[0];
    nkmd_cpu_dcd_func[`DCD_IMMEN] = immen;
    nkmd_cpu_dcd_func[`DCD_JMPEN] = jmpen;
    nkmd_cpu_dcd_func[`DCD_REPN] = !jmpen && !immen && inst_i[12];
end
endfunction

always @(posedge clk) begin
    {repn_ff, jmp_en_ff, imm_en_ff, sval_c_read_en_ff, tval_r_read_en_ff, jmprel_ff, imm_ff, alusel_ff, rdsel_ff, rtsel_ff, rssel_ff} <= nkmd_cpu_dcd_func(inst_i);
end

assign rssel_o = rssel_ff;
assign rtsel_o = rtsel_ff;
assign rdsel_o = rdsel_ff;
assign alusel_o = alusel_ff;
assign imm_o = imm_ff;
assign jmprel_o = jmprel_ff;
assign tval_r_read_en_o = tval_r_read_en_ff;
assign sval_c_read_en_o = sval_c_read_en_ff;
assign imm_en_o = imm_en_ff;
assign jmp_en_o = jmp_en_ff;
assign repn_o = repn_ff;

endmodule

module nkmd_cpu_regfile(
    input clk,
    input rst,

    input dcd_decn_i,
    input [`DCD_REGSEL_W-1:0] dcd_rssel_i,
    input [`DCD_REGSEL_W-1:0] dcd_rtsel_i,
    output [31:0] mem_rsval_o,
    output [31:0] mem_rtval_o,

    input [`DCD_REGSEL_W-1:0] wb_sel_i,
    input [31:0] wb_val_i,

    output seq_regn_is_zero_o);

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
        a_ff <= 32'd0;
        b_ff <= 32'd0;
        c_ff <= 32'd0;
        d_ff <= 32'd0;
        e_ff <= 32'd0;
        f_ff <= 32'd0;
        g_ff <= 32'd0;
        h_ff <= 32'd0;
        i_ff <= 32'd0;
        j_ff <= 32'd0;
        sl_ff <= 32'd0;
        sh_ff <= 32'd0;
        n_ff <= 32'd0;
    end else begin
        case (wb_sel_i)
        4'h0: /* NOP */;
        4'h1: a_ff <= wb_val_i;
        4'h2: b_ff <= wb_val_i;
        4'h3: c_ff <= wb_val_i;
        4'h4: d_ff <= wb_val_i;
        4'h5: e_ff <= wb_val_i;
        4'h6: f_ff <= wb_val_i;
        4'h7: g_ff <= wb_val_i;
        4'h8: h_ff <= wb_val_i;
        4'h9: i_ff <= wb_val_i;
        4'ha: j_ff <= wb_val_i;
        4'hb: /* NOP */; // ra: ret addr
        4'hc: sl_ff <= wb_val_i;
        4'hd: sh_ff <= wb_val_i;
        4'he: /* NOP */; // handled below
        4'hf: /* NOP */; // pc: program counter
        endcase

        if (wb_sel_i == 4'he)
            n_ff <= wb_val_i;
        else if (dcd_decn_i == 1'b1)
            n_ff <= n_ff - 1;
    end
end

reg regn_is_zero_ff;
always @(posedge clk) begin
    if (rst) begin
        regn_is_zero_ff <= 1'b1;
    end else begin
        if (wb_sel_i == 4'he)
            regn_is_zero_ff <= (wb_val_i == 32'b0);
        else if (dcd_decn_i == 1'b1)
            regn_is_zero_ff <= (n_ff - 1 == 0);
        else
            regn_is_zero_ff <= (n_ff == 0);
    end
end
assign seq_regn_is_zero_o = regn_is_zero_ff;

`ifdef SIMULATION
task dump;
begin
    $display("-- reg dump ------------------------------------------------------------------");
    $display("a %h b %h c %h d %h e %h f %h g %h", a_ff, b_ff, c_ff, d_ff, e_ff, f_ff, g_ff);
    $display("h %h i %h j %h sh:l %h:%h n %h", h_ff, i_ff, j_ff, sh_ff, sl_ff, n_ff);
end
endtask
`endif

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
    output c_we_o,

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

reg [31:0] r_eff_addr_ff;
reg r_read_en_ff;
always @(posedge clk) begin
    if (rst) begin
        r_eff_addr_ff <= 0;
        r_read_en_ff <= 0;
    end else begin
        r_eff_addr_ff <= mem_r_addr_i;
        r_read_en_ff <= mem_r_read_en;
    end
end
assign mem_r_data_o = r_read_en_ff ? r_data_i : r_eff_addr_ff;

assign c_data_o = 32'b0; // FIXME
assign c_addr_o = mem_c_addr_i;
assign c_we_o = 1'b0;

reg [31:0] c_eff_addr_ff;
reg c_read_en_ff;
always @(posedge clk) begin
    if (rst) begin
        c_eff_addr_ff <= 0;
        c_read_en_ff <= 0;
    end else begin
        c_eff_addr_ff <= mem_c_addr_i;
        c_read_en_ff <= mem_c_read_en;
    end
end
assign mem_c_data_o = c_read_en_ff ? c_data_i : c_eff_addr_ff;

endmodule

module nkmd_cpu_ex(
    input clk,
    input rst,

    input [31:0] rsval_i,
    input [31:0] rtval_i,
    input [`DCD_ALUSEL_W-1:0] alusel_i,
    input [`DCD_REGSEL_W-1:0] rdsel_i,

    output [`DCD_REGSEL_W-1:0] rdsel_o,
    output [31:0] val_o
    );

reg [31:0] val_ff;
assign val_o = val_ff;

always @(posedge clk) begin
    if (rst) begin
        val_ff <= 32'h0;
    end else begin
        case (alusel_i)
        `OP_ADD:
            val_ff <= rsval_i + rtval_i;
        `OP_SUB:
            val_ff <= rsval_i - rtval_i;
        `OP_OR:
            val_ff <= rsval_i | rtval_i;
        `OP_AND:
            val_ff <= rsval_i & rtval_i;
        `OP_XOR:
            val_ff <= rsval_i ^ rtval_i;
        `OP_RESERVED, `OP_CLAMP, `OP_MUL:
            val_ff <= 32'h0;
        // FIXME: OP_CLAMP OP_MUL
        endcase
    end
end

reg [`DCD_REGSEL_W-1:0] rdsel_ff;
always @(posedge clk)
    rdsel_ff <= rdsel_i;
assign rdsel_o = rdsel_ff;

endmodule

module nkmd_cpu_wb(
    input clk,
    input rst,
    
    input [`DCD_REGSEL_W-1:0] regsel_i,
    input [31:0] val_i,

    // to regfile
    output [`DCD_REGSEL_W-1:0] rf_rdsel_o,
    output [31:0] rf_val_o

    /* FIXME
    // to memaa
    output [31:0] memex_addr_o */);

assign rf_rdsel_o = regsel_i;
assign rf_val_o = val_i;

endmodule

module nkmd_cpu_seq(
    input rf_regn_is_zero_i,
    input dcd_repn_i,

    output if_stop_inc_pc_o,
    output dcd_latch_curr_output_o);

wire stall = (dcd_repn_i == 1'b1) && (rf_regn_is_zero_i == 1'b0);

assign if_stop_inc_pc_o = stall;
assign dcd_latch_curr_output_o = stall;

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

// *** Inter-stage wires ***

// IF -> DCD: Instruction Fetch -> DeCoDe
wire [31:0] if_dcd_inst;

// DCD -> MEM
wire [`DCD_REGSEL_W-1:0] dcd_mem_rssel;
wire [`DCD_REGSEL_W-1:0] dcd_mem_rtsel;
wire [`DCD_REGSEL_W-1:0] dcd_mem_rdsel;
wire [`DCD_ALUSEL_W-1:0] dcd_mem_alusel;
wire [`DCD_IMM_W-1:0] dcd_mem_imm;
wire [`DCD_JMPREL_W-1:0] dcd_mem_jmprel;
wire dcd_mem_imm_en;
wire dcd_mem_jmp_en;
wire dcd_mem_tval_r_read_en;
wire dcd_mem_sval_c_read_en;

// MEM -> EX
wire [31:0] mem_ex_sval;
wire [31:0] mem_ex_tval;
wire [`DCD_ALUSEL_W-1:0] mem_ex_alusel;
wire [`DCD_REGSEL_W-1:0] mem_ex_rdsel;

// EX -> WB
wire [`DCD_REGSEL_W-1:0] ex_wb_rdsel;
wire [31:0] ex_wb_val;

// WB -> IF
wire [31:0] wb_if_jmp_pc;
wire wb_if_jmp_pc_en;

// *** Multistage components' wires ***

// DCD -> RF
wire [`DCD_REGSEL_W-1:0] dcd_rf_rssel = dcd_mem_rssel;
wire [`DCD_REGSEL_W-1:0] dcd_rf_rtsel = dcd_mem_rtsel;
wire dcd_rf_decn;

// RF -> MEM
wire [31:0] rf_mem_rsval;
wire [31:0] rf_mem_rtval;

// WB -> RF
wire [`DCD_REGSEL_W-1:0] wb_rf_regsel;
wire [31:0] wb_rf_val;

// RF -> SEQ
wire rf_seq_regn_is_zero;

// DCD -> SEQ
wire dcd_seq_repn;

// SEQ -> IF
wire seq_if_stop_inc_pc;

// SEQ -> DCD
wire seq_dcd_latch_curr_output;

// *** Pipeline stages ***

// IF: Instruction Fetch
nkmd_cpu_if nkmd_cpu_if(
    .clk(clk), .rst(rst),
    .p_data_i(p_data_i),
    .p_addr_o(p_addr_o),
    .seq_stop_inc_pc_i(seq_if_stop_inc_pc),
    .jmp_pc_i(wb_if_jmp_pc),
    .jmp_pc_en_i(wb_if_jmp_pc_en),
    .inst_o(if_dcd_inst));

// DCD: instruction DeCoDe
wire dcd_repn_o;
nkmd_cpu_dcd nkmd_cpu_dcd(
    .clk(clk), .rst(rst),
    .inst_i(if_dcd_inst),
    .rssel_o(dcd_mem_rssel),
    .rtsel_o(dcd_mem_rtsel),
    .rdsel_o(dcd_mem_rdsel),
    .alusel_o(dcd_mem_alusel),
    .imm_o(dcd_mem_imm),
    .jmprel_o(dcd_mem_jmprel),
    .tval_r_read_en_o(dcd_mem_tval_r_read_en),
    .sval_c_read_en_o(dcd_mem_sval_c_read_en),
    .imm_en_o(dcd_mem_imm_en),
    .jmp_en_o(dcd_mem_jmp_en),
    .repn_o(dcd_repn_o));
assign dcd_rf_decn = dcd_repn_o;
assign dcd_seq_repn = dcd_repn_o;

// MEM: Memory fetch
wire [31:0] mem_r_addr_i = dcd_mem_imm_en ? dcd_mem_imm : rf_mem_rtval;
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

    .mem_r_addr_i(mem_r_addr_i),
    .mem_r_read_en(dcd_mem_tval_r_read_en),
    .mem_r_data_o(mem_ex_tval),
    .mem_c_addr_i(rf_mem_rsval),
    .mem_c_read_en(dcd_mem_sval_c_read_en),
    .mem_c_data_o(mem_ex_sval));
reg [`DCD_ALUSEL_W-1:0] mem_alusel_ff;
reg [`DCD_REGSEL_W-1:0] mem_rdsel_ff;
always @(posedge clk) begin
    mem_alusel_ff <= dcd_mem_alusel;
    mem_rdsel_ff <= dcd_mem_rdsel;
end
assign mem_ex_alusel = mem_alusel_ff;
assign mem_ex_rdsel = mem_rdsel_ff;

// EX: EXecute alu
nkmd_cpu_ex nkmd_cpu_ex(
    .clk(clk), .rst(rst),

    .rsval_i(mem_ex_sval),
    .rtval_i(mem_ex_tval),
    .alusel_i(mem_ex_alusel),
    .rdsel_i(mem_ex_rdsel),

    .rdsel_o(ex_wb_rdsel),
    .val_o(ex_wb_val));

// WB: WriteBack to mem or regfile
nkmd_cpu_wb nkmd_cpu_wb(
    .clk(clk), .rst(rst),

    .regsel_i(ex_wb_rdsel),
    .val_i(ex_wb_val),

    .rf_rdsel_o(wb_rf_regsel),
    .rf_val_o(wb_rf_val));

// *** Multistage components ***

// SEQ: SEQuencer
nkmd_cpu_seq nkmd_cpu_seq(
    /* .clk(clk), .rst(rst), */

    .rf_regn_is_zero_i(rf_seq_regn_is_zero),
    .dcd_repn_i(dcd_seq_repn),

    .if_stop_inc_pc_o(seq_if_stop_inc_pc),
    .dcd_latch_curr_output_o(seq_dcd_latch_curr_output));

// RF: Register File
nkmd_cpu_regfile nkmd_cpu_regfile(
    .clk(clk), .rst(rst),

    .dcd_decn_i(dcd_rf_decn),
    .dcd_rssel_i(dcd_rf_rssel),
    .dcd_rtsel_i(dcd_rf_rtsel),
    .mem_rsval_o(rf_mem_rsval),
    .mem_rtval_o(rf_mem_rtval),

    .wb_sel_i(wb_rf_regsel),
    .wb_val_i(wb_rf_val),

    .seq_regn_is_zero_o(rf_seq_regn_is_zero));

// MEMAA: MEMory Access Arbitrator

`ifdef SIMULATION

task print_regsel;
    input [`DCD_REGSEL_W-1:0] regsel;
begin
    case (regsel)
    4'h0: $write("c0");
    4'h1: $write("a");
    4'h2: $write("b");
    4'h3: $write("c");
    4'h4: $write("d");
    4'h5: $write("e");
    4'h6: $write("f");
    4'h7: $write("g");
    4'h8: $write("h");
    4'h9: $write("i");
    4'ha: $write("j");
    4'hb: $write("ra");
    4'hc: $write("sl");
    4'hd: $write("sh");
    4'he: $write("n");
    4'hf: $write("pc");
    default: $write("?%h", regsel);
    endcase
end
endtask

task print_alusel;
    input [`DCD_ALUSEL_W-1:0] alusel;
begin
    case (alusel)
    3'h0: $write("add");
    3'h1: $write("sub");
    3'h2: $write("or");
    3'h3: $write("and");
    3'h4: $write("xor");
    3'h5: $write("resv");
    3'h6: $write("clamp");
    3'h7: $write("mul");
    default: $write("?%h", alusel);
    endcase
end
endtask

always @(posedge clk) begin
    $display("= nkmm CPU state dump ========================================================");
    $display("IF/DCD  inst %h", if_dcd_inst);
    $write("DCD/MEM rs ");
    print_regsel(dcd_mem_rssel);
    $write(" rt ");
    print_regsel(dcd_mem_rtsel);
    $write(" rd ");
    print_regsel(dcd_mem_rdsel);
    $write(" alu ");
    print_alusel(dcd_mem_alusel);
    if (dcd_mem_imm_en)
        $write(" imm %h", dcd_mem_imm);
    else
        $write(" imm disabled");
    if (dcd_mem_jmp_en)
        $write(" jmprel %h", dcd_mem_jmprel);
    else
        $write(" jmprel disabled");
    $write(" [r->t,c->s]rden %h%h", dcd_mem_tval_r_read_en, dcd_mem_sval_c_read_en);
    $write("\n");
    $display("MEM r_addr_i %h from imm? %d", mem_r_addr_i, dcd_mem_imm_en);
    $write("MEM/EX  ");
    $write("sval %h tval %h", mem_ex_sval, mem_ex_tval);
    $write(" alusel ");
    print_alusel(mem_ex_alusel);
    $write("\n");
    $write("EX/WB   ");
    $write("rdsel ");
    print_regsel(ex_wb_rdsel);
    $write(" val %h", ex_wb_val);
    $write("\n");
    $write("WB/IF   ");
    if (wb_if_jmp_pc_en)
        $write("jmp_pc %h");
    else
        $write("jmp_pc disabled");
    $write("\n");
    $display("------------------------------------------------------------------------------");
    $display("IF fetchaddr %h", p_addr_o);
    $write("DCD/RF ");
    $write(" rssel ");
    print_regsel(dcd_rf_rssel);
    $write(" rtsel ");
    print_regsel(dcd_rf_rtsel);
    $write("\n");
    $write("RF/MEM "); 
    $write(" sval %h", rf_mem_rsval);
    $write(" tval %h", rf_mem_rtval);
    $write("\n");
    $write("WB/RF  "); 
    $write(" regsel ");
    print_regsel(wb_rf_regsel);
    $write(" val %h", wb_rf_val);
    $write("\n");
    $display("RF/SEQ  repn %h", dcd_seq_repn);
    $display("SEQ/IF  stop_inc_pc %h", seq_if_stop_inc_pc);
    $display("SEQ/DCD latch_curr_output %h", seq_dcd_latch_curr_output);
    $display("------------------------------------------------------------------------------");
    nkmd_cpu_regfile.dump();
    $display("==============================================================================");
end
`endif

endmodule
