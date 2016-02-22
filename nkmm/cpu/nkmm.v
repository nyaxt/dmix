`include "../nkmm_const.v"

module nkmm_insn_decoder(
    input [`INSN_WIDTH-1:0] insn_i,
    output mem_write_o,
    output mem_read_o,
    output [`OPSEL_WIDTH-1:0] op_sel_o,
    output [`REGSEL_WIDTH-1:0] d_sel_o,
    output [`REGSEL_WIDTH-1:0] a_sel_o,
    output [`REGSEL_WIDTH-1:0] b_sel_o,
    output [`IMM_WIDTH-1:0] imm_o,
    output imm_en_o);

assign mem_write_o = insn_i[30];
assign mem_read_o = insn_i[29];
assign d_sel_o = insn_i[28:26];
assign op_sel_o = insn_i[25:23];
assign a_sel_o = insn_i[22:20];
assign b_sel_o = insn_i[19:17];
assign imm_en_o = insn_i[16];
assign imm_o = insn_i[15:0];

endmodule

module nkmm_cpu(
    input clk,
    input rst,

    // data bus
    input [`ACCUM_WIDTH-1:0] data_i,
    output [`ACCUM_WIDTH-1:0] data_o,
    output [`ADDR_WIDTH-1:0] addr_o,
    output we_o,

    // pmem bus
    input [`INSN_WIDTH-1:0] prog_data_i,
    output [`ADDR_WIDTH-1:0] prog_addr_o);

reg [`ACCUM_WIDTH-1:0] ra_ff;
reg [`ACCUM_WIDTH-1:0] rb_ff;
reg [`ACCUM_WIDTH-1:0] rc_ff;
reg [`ACCUM_WIDTH-1:0] rd_ff;
reg [`ACCUM_WIDTH-1:0] re_ff;
reg [`ACCUM_WIDTH-1:0] sp_ff;
reg [`ADDR_WIDTH-1:0] pc_ff;

// STAGE if: Instruction Fetch
// OUTPUT:
wire [`INSN_WIDTH-1:0] if_inst;

// STAGE dcd: Decode instruction, fetch registers to be used in ALU
// OUTPUT:
reg dcd_invalid_ff;
reg dcd_mem_write_ff;
reg dcd_mem_read_ff;
reg [`OPSEL_WIDTH-1:0] dcd_op_sel_ff;
reg [`ACCUM_WIDTH-1:0] dcd_alu_a_ff;
reg [`ACCUM_WIDTH-1:0] dcd_alu_b_ff;
reg [`ACCUM_WIDTH-1:0] dcd_reg_d_ff;
reg [`REGSEL_WIDTH-1:0] dcd_d_sel_ff;

// STAGE ex: Execute ALU Operation
// OUTPUT:
reg ex_invalid_ff;
reg ex_mem_read_ff;
reg ex_mem_write_ff;
reg [`ACCUM_WIDTH-1:0] ex_alu_r_ff;
reg [`ACCUM_WIDTH-1:0] ex_reg_d_ff;
reg [`REGSEL_WIDTH-1:0] ex_d_sel_ff;

// STAGE mio: Memory read/write
// OUTPUT:
reg mio_invalid_ff;
reg [`REGSEL_WIDTH-1:0] mio_d_sel_ff;
wire [`ACCUM_WIDTH-1:0] mio_r;

// STAGE wb: Writeback
// OUTPUT:
reg wb_jump_en_ff;
reg [`ADDR_WIDTH-1:0] wb_jump_addr_ff;

// STAGE if: Instruction Fetch
// OUTPUT:
// wire [`INSN_WIDTH-1:0] if_inst;

wire dcd_invalid;
wire if_stall = dcd_invalid;

reg if_prev_stall_ff; // should degenerate with dcd_invalid_ff
always @(posedge clk) begin
    if (rst)
        if_prev_stall_ff <= 1'b1;
    else
        if_prev_stall_ff <= if_stall;
end

assign prog_addr_o = pc_ff;
always @(posedge clk) begin
    if (rst) begin
        pc_ff <= 0;
    end else begin
        if (wb_jump_en_ff) begin
            pc_ff <= wb_jump_addr_ff;
        end else if (if_stall) begin
            pc_ff <= pc_ff;
        end else begin
            pc_ff <= pc_ff + 1;
        end
    end
end

reg [`INSN_WIDTH-1:0] if_prev_inst_ff;

assign if_inst = if_prev_stall_ff ? if_prev_inst_ff : prog_data_i;
always @(posedge clk) begin
    if_prev_inst_ff <= if_inst;
end
`ifdef SIMULATION
reg [`ADDR_WIDTH-1:0] if_inst_addr_ff;
always @(posedge clk) begin
    if (if_stall)
        if_inst_addr_ff <= if_inst_addr_ff;
    else
        if_inst_addr_ff <= pc_ff;
end
`endif

// STAGE dcd: Decode instruction, fetch registers to be used in ALU
// OUTPUT:
// reg dcd_invalid_ff;
// reg dcd_mem_write_ff;
// reg dcd_mem_read_ff;
// reg [`OPSEL_WIDTH-1:0] dcd_op_sel_ff;
// reg [`ACCUM_WIDTH-1:0] dcd_alu_a_ff;
// reg [`ACCUM_WIDTH-1:0] dcd_alu_b_ff;
// reg [`ACCUM_WIDTH-1:0] dcd_reg_d_ff;
// reg [`REGSEL_WIDTH-1:0] dcd_d_sel_ff;
`ifdef SIMULATION
reg [`ADDR_WIDTH-1:0] dcd_inst_addr_ff;
always @(posedge clk) begin
    dcd_inst_addr_ff <= if_inst_addr_ff;
end
`endif

//  instruction decode
wire dcd_mem_write;
wire dcd_mem_read;
wire [`OPSEL_WIDTH-1:0] dcd_op_sel;
wire [`REGSEL_WIDTH-1:0] dcd_d_sel;
wire [`REGSEL_WIDTH-1:0] dcd_a_sel;
wire [`REGSEL_WIDTH-1:0] dcd_b_sel;
wire [`IMM_WIDTH-1:0] dcd_imm;
wire dcd_imm_en;
nkmm_insn_decoder decoder(
    .insn_i(if_inst),
    .mem_write_o(dcd_mem_write),
    .mem_read_o(dcd_mem_read),
    .op_sel_o(dcd_op_sel),
    .d_sel_o(dcd_d_sel),
    .a_sel_o(dcd_a_sel),
    .b_sel_o(dcd_b_sel),
    .imm_o(dcd_imm),
    .imm_en_o(dcd_imm_en));

parameter SEL_R0 = 3'b000;
parameter SEL_RA = 3'b001;
parameter SEL_RB = 3'b010;
parameter SEL_RC = 3'b011;
parameter SEL_RD = 3'b100;
parameter SEL_RE = 3'b101;
parameter SEL_SP = 3'b110;
parameter SEL_PC = 3'b111;
function [`ACCUM_WIDTH-1:0] dcd_reg_sel(
    input [2:0] sel,
    input [`ACCUM_WIDTH-1:0] r1,
    input [`ACCUM_WIDTH-1:0] r2,
    input [`ACCUM_WIDTH-1:0] r3,
    input [`ACCUM_WIDTH-1:0] r4,
    input [`ACCUM_WIDTH-1:0] r5,
    input [`ACCUM_WIDTH-1:0] sp,
    input [`ADDR_WIDTH-1:0] pc);
    case (sel)
        SEL_R0:
            dcd_reg_sel = 0;
        SEL_RA:
            dcd_reg_sel = r1;
        SEL_RB:
            dcd_reg_sel = r2;
        SEL_RC:
            dcd_reg_sel = r3;
        SEL_RD:
            dcd_reg_sel = r4;
        SEL_RE:
            dcd_reg_sel = r5;
        SEL_SP:
            dcd_reg_sel = sp;
        SEL_PC: begin
            dcd_reg_sel[`ACCUM_WIDTH-1:`ADDR_WIDTH] = 0;
            dcd_reg_sel[`ADDR_WIDTH-1:0] = pc;
        end
    endcase
endfunction

wire dcd_dcd_conflict = dcd_invalid_ff == 1'b0 && dcd_d_sel_ff != 0 && (dcd_d_sel_ff == dcd_a_sel || dcd_d_sel_ff == dcd_b_sel);
wire dcd_dcd_conflict_mw = dcd_mem_write == 1'b1 && dcd_invalid_ff == 1'b0 && dcd_d_sel_ff != 0 && dcd_d_sel_ff == dcd_d_sel;
wire dcd_ex_conflict = ex_invalid_ff == 1'b0 && ex_d_sel_ff != 0 && (ex_d_sel_ff == dcd_a_sel || ex_d_sel_ff == dcd_b_sel);
wire dcd_ex_conflict_mw = dcd_mem_write == 1'b1 && ex_invalid_ff == 1'b0 && ex_d_sel_ff != 0 && ex_d_sel_ff == dcd_d_sel;
wire dcd_mio_conflict = mio_invalid_ff == 1'b0 && mio_d_sel_ff != 0 && (mio_d_sel_ff == dcd_a_sel || mio_d_sel_ff == dcd_b_sel);
wire dcd_mio_conflict_mw = dcd_mem_write == 1'b1 && mio_invalid_ff == 1'b0 && mio_d_sel_ff != 0 && mio_d_sel_ff == dcd_d_sel;
assign dcd_invalid = dcd_dcd_conflict || dcd_dcd_conflict_mw || dcd_ex_conflict || dcd_ex_conflict_mw || dcd_mio_conflict || dcd_mio_conflict_mw;

always @(posedge clk) begin
    if (rst) begin
        dcd_invalid_ff <= 1'b1;
        dcd_mem_write_ff <= 0;
        dcd_mem_read_ff <= 0;
        dcd_op_sel_ff <= 0;
        dcd_alu_a_ff <= 0;
        dcd_alu_b_ff <= 0;
        dcd_reg_d_ff <= 0;
        dcd_d_sel_ff <= 0;
    end else begin
        dcd_invalid_ff <= dcd_invalid;
        dcd_mem_write_ff <= dcd_mem_write;
        dcd_mem_read_ff <= dcd_mem_read;
        dcd_op_sel_ff <= dcd_op_sel;
        dcd_alu_a_ff <= dcd_reg_sel(dcd_a_sel, ra_ff, rb_ff, rc_ff, rd_ff, re_ff, sp_ff, pc_ff);
        if (dcd_imm_en) begin
            dcd_alu_b_ff[`ACCUM_WIDTH-1:`IMM_WIDTH] <= 0;
            dcd_alu_b_ff[`IMM_WIDTH-1:0] <= dcd_imm;
        end else begin
            dcd_alu_b_ff <= dcd_reg_sel(dcd_b_sel, ra_ff, rb_ff, rc_ff, rd_ff, re_ff, sp_ff, pc_ff);
        end
        dcd_reg_d_ff <= dcd_reg_sel(dcd_d_sel, ra_ff, rb_ff, rc_ff, rd_ff, re_ff, sp_ff, pc_ff);
        dcd_d_sel_ff <= dcd_d_sel;
    end
end

// STAGE ex: Execute ALU Operation
// OUTPUT:
// reg ex_invalid_ff;
// reg ex_mem_read_ff;
// reg ex_mem_write_ff;
// reg [`ACCUM_WIDTH-1:0] ex_alu_r_ff;
// reg [`ACCUM_WIDTH-1:0] ex_reg_d_ff;
// reg [`REGSEL_WIDTH-1:0] ex_d_sel_ff;
`ifdef SIMULATION
reg [`ADDR_WIDTH-1:0] ex_inst_addr_ff;
always @(posedge clk) begin
    ex_inst_addr_ff <= dcd_inst_addr_ff;
end
`endif

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
        ex_invalid_ff <= 1'b0;
        ex_mem_read_ff <= 1'b0;
        ex_mem_write_ff <= 1'b0;
        ex_alu_r_ff <= 0;
        ex_reg_d_ff <= 0;
        ex_d_sel_ff <= 0;
    end else begin
        ex_invalid_ff <= dcd_invalid_ff;
        ex_mem_read_ff <= dcd_mem_read_ff;
        ex_mem_write_ff <= dcd_mem_write_ff;
        ex_alu_r_ff <= alu(dcd_op_sel_ff, dcd_alu_a_ff, dcd_alu_b_ff);
        ex_reg_d_ff <= dcd_reg_d_ff;
        ex_d_sel_ff <= dcd_d_sel_ff;
    end
end

// STAGE mio: Memory read/write
// OUTPUT:
// reg mio_invalid_ff;
// reg [`REGSEL_WIDTH-1:0] mio_d_sel_ff;
// wire [`ACCUM_WIDTH-1:0] mio_r;
`ifdef SIMULATION
reg [`ADDR_WIDTH-1:0] mio_inst_addr_ff;
always @(posedge clk) begin
    mio_inst_addr_ff <= ex_inst_addr_ff;
end
`endif

reg mio_mem_read_ff;
reg [`ACCUM_WIDTH-1:0] mio_alu_r_ff;

assign data_o[`ACCUM_WIDTH-1:0] = ex_reg_d_ff;
assign addr_o[`ADDR_WIDTH-1:0] = ex_alu_r_ff[`ADDR_WIDTH-1:0];
assign we_o = ex_mem_write_ff && !ex_invalid_ff;

always @(posedge clk) begin
    if (rst) begin
        mio_invalid_ff <= 1'b0;
        mio_d_sel_ff <= 0;
        mio_mem_read_ff <= 1'b0;
        mio_alu_r_ff <= 0;
    end else begin
        mio_invalid_ff <= ex_invalid_ff;
        mio_d_sel_ff <= ex_d_sel_ff;
        mio_mem_read_ff <= ex_mem_read_ff;
        mio_alu_r_ff <= ex_alu_r_ff;
    end
end

assign mio_r[`ACCUM_WIDTH-1:0] = mio_mem_read_ff ? data_i : mio_alu_r_ff;

// STAGE wb: Writeback
// OUTPUT:
// reg wb_jump_en_ff;
// reg [`ADDR_WIDTH-1:0] wb_jump_addr_ff;
`ifdef SIMULATION
reg [`ADDR_WIDTH-1:0] wb_inst_addr_ff;
always @(posedge clk) begin
    wb_inst_addr_ff <= mio_inst_addr_ff;
end
`endif

always @(posedge clk) begin
    if (rst) begin
        wb_jump_en_ff <= 1'b0;
        wb_jump_addr_ff <= 0;

        ra_ff <= 0;
        rb_ff <= 0;
        rc_ff <= 0;
        rd_ff <= 0;
        re_ff <= 0;
        sp_ff <= 0;
    end else if (!mio_invalid_ff) begin
        wb_jump_en_ff <= (mio_d_sel_ff == SEL_PC) ? 1'b1 : 1'b0;
        wb_jump_addr_ff <= mio_r[`ADDR_WIDTH-1:0];

        case (mio_d_sel_ff)
            SEL_RA:
                ra_ff <= mio_r;
            SEL_RB:
                rb_ff <= mio_r;
            SEL_RC:
                rc_ff <= mio_r;
            SEL_RD:
                rd_ff <= mio_r;
            SEL_RE:
                re_ff <= mio_r;
            SEL_SP:
                sp_ff <= mio_r;
        endcase
    end
end

`ifdef SIMULATION
always @(posedge clk) begin
    $display("= nkmm CPU state dump ========================================================");
    $display(" IF  in. pc_ff: %h", pc_ff);
    $display(" IF out. addr: %h stall: %b inst: %h", if_inst_addr_ff, if_stall, if_inst);
    $display("DCD  in. mr,w: %b,%b opsel: %h d,a,bsel: %h,%h,%h imm: %h,%b", dcd_mem_read, dcd_mem_write, dcd_op_sel, dcd_d_sel, dcd_a_sel, dcd_b_sel, dcd_imm, dcd_imm_en);
    $display("DCD out. addr: %h inval: %b mr,w: %b,%b opsel: %h, alu_a,b: %h,%h, reg_d,d_sel: %h,%h", dcd_inst_addr_ff, dcd_invalid_ff, dcd_mem_read_ff, dcd_mem_write_ff, dcd_op_sel_ff, dcd_alu_a_ff, dcd_alu_b_ff, dcd_reg_d_ff, dcd_d_sel_ff);
    $display(" EX out. addr: %h inval: %b mr,w: %b,%b alu_r: %h, reg_d,d_sel: %h,%h", ex_inst_addr_ff, ex_invalid_ff, ex_mem_read_ff, ex_mem_write_ff, ex_alu_r_ff, ex_reg_d_ff, ex_d_sel_ff);
    $display("MIO  in. data_o: %h addr_o: %h we_o: %b", data_o, addr_o, we_o);
    $display("MIO out. addr: %h inval: %b mr: %b d_sel: %h alu_r: %h mio_r: %h", mio_inst_addr_ff, mio_invalid_ff, mio_mem_read_ff, mio_d_sel_ff, mio_alu_r_ff, mio_r);
    $display(" WB out. addr: %h jump_en: %b jump_addr: %h", wb_inst_addr_ff, wb_jump_en_ff, wb_jump_addr_ff);
    $display("==============================================================================");
end
`endif

endmodule
