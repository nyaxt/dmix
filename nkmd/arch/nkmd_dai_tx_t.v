`timescale 1ns / 1ps

module nkmd_dai_tx_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg rst;
reg pop;

reg [31:0] nkmd_data_i;
wire [31:0] nkmd_data_o;
reg [31:0] nkmd_addr;
reg nkmd_we;

nkmd_dai_tx uut(
    .clk(clk),
    .rst(rst),

    .tx_pop_i(pop),

    .data_i(nkmd_data_i),
    .data_o(nkmd_data_o),
    .addr_i(nkmd_addr),
    .we_i(nkmd_we));

task queue;
    input wire [23:0] sample;
    begin
        nkmd_addr = 32'h0000d001;
        nkmd_data_i = {8'h0, sample};
        nkmd_we = 1'b1;
        #(TCLK);
        nkmd_we = 1'b0;
    end
endtask

task trigger_pop;
    begin
        pop = 1'b1;
        #(TCLK);
        pop = 1'b0;
    end
endtask

reg [31:0] i;
initial begin
    $dumpfile("nkmd_dai_tx_t.lxt");
    $dumpvars(0, nkmd_dai_tx_t);

    pop = 0;
    nkmd_data_i = 32'h0;
    nkmd_addr = 32'h0;
    nkmd_we = 1'b0;

    rst = 1'b1;
    #(TCLK);
    rst = 1'b0;
    #(TCLK);

    nkmd_addr = 32'h0000d001;
    #(TCLK);
    $display("expect %h, actual %h : queued_ff peek", 6'h0, uut.queued_ff);
    $display("expect %h, actual %h : queued_ff ram read", 32'h0, nkmd_data_o);

    queue(24'hcafebb);

    nkmd_addr = 32'h0000e000;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf ram read", 32'h00cafebb, nkmd_data_o);

    nkmd_addr = 32'h0000d001;
    #(TCLK);
    $display("expect %h, actual %h : queued_ff peek", 6'h1, uut.queued_ff);
    $display("expect %h, actual %h : queued_ff ram read", 32'h1, nkmd_data_o);

    trigger_pop();

    nkmd_addr = 32'h0000d001;
    #(TCLK);
    $display("expect %h, actual %h : queued_ff peek", 6'h0, uut.queued_ff);
    $display("expect %h, actual %h : queued_ff ram read", 32'h0, nkmd_data_o);

    for (i = 0; i < 63; i = i + 1) begin
        queue(i);
    end
    $display("expect %h, actual %h : queued_ff peek", 6'd63, uut.queued_ff);
    for (i = 0; i < 63; i = i + 1) begin
        trigger_pop();
    end
    for (i = 63; i < 80; i = i + 1) begin
        queue(i);
    end
    for (i = 63; i < 80; i = i + 1) begin
        trigger_pop();
    end

    #(TCLK*3);
    $finish(2);
end

always @(posedge clk) begin
    if (uut.tx_ack_o)
        $display("tx emit sample %h", uut.tx_data_o);
end

endmodule
