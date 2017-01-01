`timescale 1ns / 1ps

module nkmd_ddr3_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg rst;

reg [31:0] nkmd_data_i;
wire [31:0] nkmd_data_o;
reg [15:0] nkmd_addr;
reg nkmd_we;

reg [31:0] mig_rd_data;
reg [6:0] mig_rd_count;

nkmd_ddr3 uut(
    .clk(clk),
    .rst(rst),

    .mig_cmd_empty(1'b1),
    .mig_cmd_full(1'b0),
    .mig_wr_full(1'b1),
    .mig_wr_empty(1'b0),
    // .mig_wr_count,
    // .mig_wr_underrun,
    // .mig_wr_error,
    .mig_rd_data(mig_rd_data),
    // .mig_rd_full,
    // .mig_rd_empty,
    .mig_rd_count(mig_rd_count),
    // .mig_rd_overflow,
    // .mig_rd_error,

    .data_i(nkmd_data_i),
    .data_o(nkmd_data_o),
    .addr_i(nkmd_addr),
    .we_i(nkmd_we));

task nkmd_write;
    input [15:0] addr;
    input [31:0] data;
    begin
        nkmd_addr = addr;
        nkmd_data_i = data;
        nkmd_we = 1'b1;
        #(TCLK);
        nkmd_we = 1'b0;
    end
endtask

task nkmd_read;
    input [15:0] addr;
    begin
        nkmd_addr = addr;
        #(TCLK);
        $display("read addr: %x data: %x", nkmd_addr, nkmd_data_o);
    end
endtask

reg [31:0] i;
initial begin
    $dumpfile("nkmd_ddr3_t.lxt");
    $dumpvars(0, nkmd_ddr3_t);

    nkmd_data_i = 32'h0;
    nkmd_addr = 16'h0;
    nkmd_we = 1'b0;

    mig_rd_data = 32'hdeadbeef;
    mig_rd_count = 6'h00;

    rst = 1'b1;
    #(TCLK);
    rst = 1'b0;
    #(TCLK*10);

    nkmd_write(16'h1080, 32'h00000001);
    nkmd_write(16'h1081, 32'h00000002);
    nkmd_write(16'h1082, 32'h00000003);
    nkmd_write(16'h1083, 32'h00000004);
    nkmd_write(16'h1084, 32'h00000005);
    nkmd_write(16'h1085, 32'h00000006);
    nkmd_write(16'h1086, 32'h00000007);
    nkmd_write(16'h1087, 32'h00000008);
    nkmd_write(16'hc100, 32'h0abcdefc);
    nkmd_write(16'hc101, 32'h00000080);
    nkmd_write(16'hc102, 32'h00000003);
    nkmd_write(16'hc103, 32'h00000001);
    #(TCLK*32);

    nkmd_write(16'hc100, 32'hadadadad);
    nkmd_write(16'hc101, 32'h000000c0);
    nkmd_write(16'hc102, 32'h00000001);
    nkmd_write(16'hc103, 32'h00000000);
    #(TCLK*10);
    mig_rd_count = 6'h02;
    #(TCLK);
    mig_rd_count = 6'h01;
    mig_rd_data = 32'hcafebabe;
    #(TCLK);
    mig_rd_count = 6'h00;
    #(TCLK*10);

    nkmd_read(16'h10c0);
    nkmd_read(16'h10c1);
    nkmd_read(16'h10c2);

    $finish(2);
end

endmodule
