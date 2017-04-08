`timescale 1ns / 1ps

module simple_ddr3_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg rst;

reg [27:0] addr;
reg [31:0] wdata;
reg we;
reg pop;

wire [31:0] mig_rd_data = 32'h12345678;
reg [6:0] mig_rd_count;

simple_ddr3 uut(
    .clk(clk),
    .rst(rst),

    .addr_i(addr),
    .data_i(wdata),
    .we_i(we),
    .pop_i(pop),

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
    .mig_rd_count(mig_rd_count)
    // .mig_rd_overflow,
    // .mig_rd_error,
    );

initial begin
    $dumpfile("simple_ddr3_t.lxt");
    $dumpvars(0, simple_ddr3_t);

    addr = 28'h1234567;
    wdata = 32'hdeadbeef;
    we = 1'b0;
    pop = 1'b0;

    rst = 1'b1;
    #(TCLK);
    rst = 1'b0;
    mig_rd_count = 'b0;
    #(TCLK*3);
    we = 1'b1;
    #(TCLK);
    we = 1'b0;
    #(TCLK*10);

    pop = 1'b1; 
    #(TCLK);
    pop = 1'b0;
    #(TCLK*10);

    $finish(2);
end

always @(posedge clk) begin
    if (uut.mig_cmd_en == 1'b1 && uut.mig_cmd_instr[0] == 1'b1) begin
        #(TCLK*5)
        mig_rd_count = 'd1;
        #(TCLK)
        mig_rd_count = 'd0;
    end
end

endmodule
