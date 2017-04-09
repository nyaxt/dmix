`timescale 1ns / 1ps
`default_nettype none

module csr_spi_t;

// ins
reg clk;
reg rst;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg sck;
parameter TCLK_SCK = 80;
reg mosi;
reg ss;

parameter NUM_CH = 8;
parameter NUM_SPDIF_IN = 3;
parameter NUM_RATE = 5;

wire [(NUM_RATE*NUM_SPDIF_IN-1):0] rate_i = {5'b00001, 5'b00100, 5'b10000};
wire [(192*NUM_SPDIF_IN-1):0] udata_i = {NUM_SPDIF_IN{192'h0102030405060708090a0b0c0d0e0f1011121314151617}};
wire [(192*NUM_SPDIF_IN-1):0] cdata_i = {NUM_SPDIF_IN{192'h0102030405060708090a0b0c0d0e0f1011121314151617}};

wire [27:0] csr_dram_addr;
wire [31:0] csr_dram_data;
wire csr_dram_we;
wire csr_dram_pop;
wire [31:0] dram_csr_data;
wire dram_csr_ack;
wire dram_csr_busy;

wire [31:0] mig_rd_data = 32'h12345678;
reg [6:0] mig_rd_count;

simple_ddr3 dram(
    .clk(clk),
    .rst(rst),

    .addr_i(csr_dram_addr),
    .data_i(csr_dram_data),
    .we_i(csr_dram_we),
    .pop_i(csr_dram_pop),
    .data_o(dram_csr_data),
    .ack_o(dram_csr_ack),
    .busy_o(dram_csr_busy),

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
always @(posedge clk) begin
    if (dram.mig_cmd_en == 1'b1 && dram.mig_cmd_instr[0] == 1'b1) begin
        #(TCLK*5)
        mig_rd_count = 'd1;
        #(TCLK)
        mig_rd_count = 'd0;
    end
end

csr_spi uut(
    .clk(clk), .rst(rst),

    .sck(sck),
    .mosi(mosi),
    .ss(ss),

    .rate_i(rate_i), .udata_i(udata_i), .cdata_i(cdata_i),
    
    .dram0_addr_o(csr_dram_addr),
    .dram0_data_o(csr_dram_data),
    .dram0_we_o(csr_dram_we),
    .dram0_pop_o(csr_dram_pop),
    .dram0_data_i(dram_csr_data),
    .dram0_ack_i(dram_csr_ack),
    .dram0_busy_i(dram_csr_busy));

task spi_cycle;
    input [7:0] data;
    begin
        #(TCLK_SCK/2);
        ss = 0;
        #(TCLK_SCK/2);
        mosi = data[7];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[6];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[5];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[4];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[3];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[2];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[1];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        mosi = data[0];
        sck = 0; #(TCLK_SCK/2);
        sck = 1; #(TCLK_SCK/2);
        #(TCLK_SCK/2);
        ss = 1;
        #(TCLK_SCK/2);
    end
endtask

initial begin
    $dumpfile("csr_spi_t.lxt");
    $dumpvars(0, csr_spi_t);

    sck = 0;
    mosi = 0;
    ss = 1;

    rst = 0;
    #(TCLK);
    rst = 1;
    #(TCLK);
    rst = 0;

    uut.csr.vol_ff = 64'h0123456789abcdef;
    $display("vol_ff: %x", uut.csr.vol_ff);

    #(TCLK*3);
    spi_cycle(8'b1_01_0_0000);
    spi_cycle(8'h03);
    spi_cycle(8'h99);
    spi_cycle(8'h00); // NOP padding

    #(TCLK*3);
    $display("after csr[12'h003] <= 8'h99");
    $display("vol_ff: %x", uut.csr.vol_ff);

    #(TCLK*3);
    spi_cycle({4'b0_01_0, 4'h8}); // high 8
    spi_cycle(8'h01); //             low 01
    spi_cycle(8'h00); // read result 12'h801
    spi_cycle(8'h00); // NOP padding
    #(TCLK*3);
    $display("---");
    #(TCLK*3);
    spi_cycle({4'b0_10_0, 4'h9}); // high 9
    spi_cycle(8'h00); //             low 00
    spi_cycle(8'h00); // read result 000
    spi_cycle(8'h00); // read result 001
    spi_cycle(8'h00); // read result 002
    spi_cycle(8'h00); // read result 003
    spi_cycle(8'h00); // NOP padding
    #(TCLK*3);
    $display("---");
    #(TCLK*3);
    spi_cycle(8'b1_10_0_0000); 
    spi_cycle(8'h04); // offset
    spi_cycle(8'hef); // data[4]
    spi_cycle(8'hbe); // data[5]
    spi_cycle(8'had); // data[6]
    spi_cycle(8'hde); // data[7]
    #(TCLK*3);
    $display("vol_ff: %x", uut.csr.vol_ff);
    $display("--- PROM write begin");
    #(TCLK*3);
    spi_cycle(8'b1_01_1_0000); // high 0
    spi_cycle(8'h00); // mid 00
    spi_cycle(8'h00); // low 00
    spi_cycle(8'hef); //
    spi_cycle(8'hbe); //
    spi_cycle(8'had); //
    spi_cycle(8'hde); //
    // write prom[20'h00000] => 32'hdeadbeef
    #(TCLK*3);
    $display("--- NKMD dbgin[3] => 8'hac");
    #(TCLK*3);
    spi_cycle({4'b1_01_0, 4'h6});
    spi_cycle(8'h03); // offset
    spi_cycle(8'hac);
    #(TCLK*3);
    $display("--- NKMD rst => 1");
    #(TCLK*3);
    spi_cycle({4'b1_01_0, 4'h4});
    spi_cycle(8'h00); // offset
    spi_cycle(8'h01);
    #(TCLK*3);
    $display("--- NKMD rst => 0");
    #(TCLK*3);
    spi_cycle({4'b1_01_0, 4'h4});
    spi_cycle(8'h00); // offset
    spi_cycle(8'h00);
    #(TCLK*3);
    $display("--- end");
    #(TCLK*3);

    spi_cycle(8'h0f); // special
    spi_cycle({4'b1_10_0, 4'h0}); // we, burst 4, special 0 -> dram 1
    spi_cycle(8'h12); // addr ms byte
    spi_cycle(8'h34);
    spi_cycle(8'h56);
    spi_cycle(8'h78); // addr ls byte
    spi_cycle(8'hef); // data ms byte
    spi_cycle(8'hbe);
    spi_cycle(8'had);
    spi_cycle(8'hde); // data ls byte
    spi_cycle(8'hef); // data ms byte
    spi_cycle(8'hbe);
    spi_cycle(8'had);
    spi_cycle(8'hde); // data ls byte
    spi_cycle(8'hef); // data ms byte
    spi_cycle(8'hbe);
    spi_cycle(8'had);
    spi_cycle(8'hde); // data ls byte
    spi_cycle(8'hef); // data ms byte
    spi_cycle(8'hbe);
    spi_cycle(8'had);
    spi_cycle(8'hde); // data ls byte
    spi_cycle(8'h00);
    #(TCLK*3);
    $display("--- end");
    #(TCLK*3);

    // write dram[0x12345678] => 32'hdeadbeef
    $finish(2);
end

always @(posedge clk) begin
    if(uut.spi_trx.ack_i)
        $display("spi data tx: %x", uut.spi_trx.data_i);
    if(uut.spi_trx.ack_pop_o)
        $display("spi data rx: %x", uut.spi_trx.data_o);
end

always @(posedge uut.nkmd_rst_o)
    $display("nkmd_rst_o posedge");
always @(negedge uut.nkmd_rst_o)
    $display("nkmd_rst_o negedge");

always @(posedge uut.csr.clk)
    if (uut.csr.ack_i)
        $display("csr write addr: %x data: %x", uut.csr.addr_i, uut.csr.data_i);

endmodule
`default_nettype wire
