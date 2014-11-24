`include "nkmm_const.v"

module nkmm_insn_decoder(
    input [`PROG_WIDTH-1:0] insn_i,
    output mem_write_o,
    output [`OPSEL_WIDTH-1:0] op_sel_o,
    output [`REGSEL_WIDTH-1:0] d_sel_o,
    output [`REGSEL_WIDTH-1:0] a_sel_o,
    output [`REGSEL_WIDTH-1:0] b_sel_o,
    output [`IMM_WIDTH-1:0] imm_o,
    output imm_en_o);

assign mem_write_o = insn_i[30];
assign d_sel_o = insn_i[29:27];
assign op_sel_o = insn_i[26:24];
assign a_sel_o = insn_i[23:21];
assign b_sel_o = insn_i[20:18];
assign imm_en_o = insn_i[17];
assign imm_o = insn_i[15:0];

endmodule

module nkmm_cpu(
    input clk,
    input rst,

    // data bus
    input [`ACCUM_WIDTH-1:0] data_i,
    output [`ACCUM_WIDTH-1:0] data_o,
    output [`ADDR_WIDTH-1:0] addr_i,
    output [`ADDR_WIDTH-1:0] addr_o,
    output we_o,

    // pmem bus
    input [`PROG_WIDTH-1:0] prog_data_i,
    output [`ADDR_WIDTH-1:0] prog_addr_o);

reg [`ACCUM_WIDTH-1:0] r0_ff;
reg [`ACCUM_WIDTH-1:0] r1_ff;
reg [`ACCUM_WIDTH-1:0] r2_ff;
reg [`ACCUM_WIDTH-1:0] r3_ff;
reg [`ACCUM_WIDTH-1:0] r4_ff;
reg [`ACCUM_WIDTH-1:0] r5_ff;
reg [`ACCUM_WIDTH-1:0] r6_ff;
reg [`ADDR_WIDTH-1:0] pc_ff;

// STAGE if: Instruction Fetch
// OUTPUT:
wire [`PROG_WIDTH-1:0] if_inst = prog_data_i;

assign prog_addr_o = pc_ff;
always @(posedge clk) begin
    if (rst) begin
        pc_ff <= 0;
    end else begin
        pc_ff <= pc_ff + 1;
    end
end

// STAGE dcd: Decode instruction, fetch registers to be used in ALU
// OUTPUT:
reg [`OPSEL_WIDTH-1:0] dcd_opsel_ff;
reg [`ACCUM_WIDTH-1:0] dcd_alu_a_ff;
reg [`ACCUM_WIDTH-1:0] dcd_alu_b_ff;

//  instruction decode
wire dcd_mem_write;
wire [`OPSEL_WIDTH-1:0] dcd_op_sel;
wire [`REGSEL_WIDTH-1:0] dcd_d_sel;
wire [`REGSEL_WIDTH-1:0] dcd_a_sel;
wire [`REGSEL_WIDTH-1:0] dcd_b_sel;
wire [`IMM_WIDTH-1:0] dcd_imm;
wire dcd_imm_en;
nkmm_insn_decoder decoder(
    .insn_i(if_inst),
    .mem_write_o(alu_mem_write),
    .op_sel_o(dcd_op_sel),
    .d_sel_o(dcd_d_sel),
    .a_sel_o(dcd_a_sel),
    .b_sel_o(dcd_b_sel),
    .imm_o(dcd_imm),
    .imm_en_o(dcd_imm_en));

parameter SEL_R0 = 3'b000;
parameter SEL_R1 = 3'b001;
parameter SEL_R2 = 3'b010;
parameter SEL_R3 = 3'b011;
parameter SEL_R4 = 3'b100;
parameter SEL_R5 = 3'b101;
parameter SEL_R6 = 3'b110;
parameter SEL_PC = 3'b111;
function [`ACCUM_WIDTH-1:0] dcd_reg_sel(
    input [2:0] sel,
    input [`ACCUM_WIDTH-1:0] r0,
    input [`ACCUM_WIDTH-1:0] r1,
    input [`ACCUM_WIDTH-1:0] r2,
    input [`ACCUM_WIDTH-1:0] r3,
    input [`ACCUM_WIDTH-1:0] r4,
    input [`ACCUM_WIDTH-1:0] r5,
    input [`ACCUM_WIDTH-1:0] r6,
    input [`ADDR_WIDTH-1:0] pc);
    case (sel)
        SEL_R0:
            dcd_reg_sel = r0;
        SEL_R1:
            dcd_reg_sel = r1;
        SEL_R2:
            dcd_reg_sel = r2;
        SEL_R3:
            dcd_reg_sel = r3;
        SEL_R4:
            dcd_reg_sel = r4;
        SEL_R5:
            dcd_reg_sel = r5;
        SEL_R6:
            dcd_reg_sel = r6;
        SEL_PC: begin
            dcd_reg_sel[`ACCUM_WIDTH-1:`ADDR_WIDTH] = 0;
            dcd_reg_sel[`ADDR_WIDTH-1:0] = pc;
        end
    endcase
endfunction

always @(posedge clk) begin
    if (rst) begin
        dcd_alu_a_ff <= 0;
        dcd_alu_b_ff <= 0;
    end else begin
        dcd_alu_a_ff <= dcd_reg_sel(dcd_a_sel, r0_ff, r1_ff, r2_ff, r3_ff, r4_ff, r5_ff, r6_ff, pc_ff);
        if (dcd_imm_en) begin
            dcd_alu_b_ff[`ACCUM_WIDTH-1:`IMM_WIDTH] <= 0;
            dcd_alu_b_ff[`IMM_WIDTH-1:0] <= dcd_imm;
        end else begin
            dcd_alu_b_ff <= dcd_reg_sel(dcd_b_sel, r0_ff, r1_ff, r2_ff, r3_ff, r4_ff, r5_ff, r6_ff, pc_ff);
        end
    end
end

// STAGE ex: Execute ALU Operation
// OUTPUT:
reg [`ACCUM_WIDTH-1:0] ex_alu_r_ff;

function [`ACCUM_WIDTH-1:0] alu(
    input [`OPSEL_WIDTH-1:0] opsel,
    input [`ACCUM_WIDTH-1:0] a,
    input [`ACCUM_WIDTH-1:0] b);
reg shl;
reg [1:0] shi;
begin
    shl = b[2];
    shi = b[1:0];
    case (opsel)
        `OP_ADD: alu = dcd_alu_a_ff + dcd_alu_b_ff;
        `OP_SUB: alu = dcd_alu_a_ff - dcd_alu_b_ff; // FIXME: should use adder: a - b = a + ~b + 1
        `OP_OR:  alu = dcd_alu_a_ff | dcd_alu_b_ff;
        `OP_AND: alu = dcd_alu_a_ff & dcd_alu_b_ff;
        `OP_XOR: alu = dcd_alu_a_ff ^ dcd_alu_b_ff;
        `OP_NOT: alu = ~dcd_alu_b_ff;
        `OP_SHI:
            case (shi)
            `SHI_1: alu = shl ? (dcd_alu_a_ff << 1) : (dcd_alu_a_ff >> 1);
            `SHI_2: alu = shl ? (dcd_alu_a_ff << 2) : (dcd_alu_a_ff >> 2);
            `SHI_4: alu = shl ? (dcd_alu_a_ff << 4) : (dcd_alu_a_ff >> 4);
            `SHI_8: alu = shl ? (dcd_alu_a_ff << 8) : (dcd_alu_a_ff >> 8);
            endcase
    endcase
end
endfunction

always @(posedge clk) begin
    if (rst) begin
        ex_alu_r_ff <= 0;
    end else begin
        ex_alu_r_ff <= alu(dcd_opsel_ff, dcd_alu_a_ff, dcd_alu_b_ff);
    end
end

endmodule
