`timescale 1ns / 1ps

module nkmd_dai_t;

// ins
reg clk;
parameter TCLK = 20;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg rst;
reg [23:0] data_ff;
reg ack_ff;

reg [31:0] nkmd_data_i;
wire [31:0] nkmd_data_o;
reg [31:0] nkmd_addr;
reg nkmd_we;

nkmd_dai_rx uut(
    .clk(clk),
    .rst(rst),

    .rx_data_i(data_ff),
    .rx_ack_i(ack_ff),

    .data_i(nkmd_data_i),
    .data_o(nkmd_data_o),
    .addr_i(nkmd_addr),
    .we_i(nkmd_we));

task push_sample;
    input [23:0] data;
    begin
        data_ff = data;
        ack_ff = 1'b1;
        #(TCLK);
        data_ff = 24'h0;
        ack_ff = 1'b0;
    end
endtask

task shift;
    begin
        nkmd_addr = 32'h0000d000;
        nkmd_we = 1'b1;
        #(TCLK);
        nkmd_we = 1'b0;
    end
endtask

reg [31:0] i;
initial begin
    $dumpfile("nkmd_dai_t.lxt");
    $dumpvars(0, nkmd_dai_t);

    data_ff = 24'h0;
    ack_ff = 1'b0;
    nkmd_data_i = 32'h0;
    nkmd_addr = 32'h0;
    nkmd_we = 1'b0;

    rst = 1'b1;
    #(TCLK);
    rst = 1'b0;
    #(TCLK);

    nkmd_addr = 32'h0000d000;
    #(TCLK);
    $display("expect %h, actual %h : unread_ff peek", 6'h0, uut.unread_ff);
    $display("expect %h, actual %h : unread_ff ram read", 32'h0, nkmd_data_o);

    push_sample(24'hcafebb);

    nkmd_addr = 32'h0000d000;
    #(TCLK);
    $display("expect %h, actual %h : unread_ff peek", 6'h1, uut.unread_ff);
    $display("expect %h, actual %h : unread_ff ram read", 32'h1, nkmd_data_o);

    nkmd_addr = 32'h0000f000;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[0] ram read", 32'h00cafebb, nkmd_data_o);

    push_sample(24'hbeef00);
    push_sample(24'hbeef01);
    push_sample(24'hbeef02);
    push_sample(24'hbeef03);
    push_sample(24'hbeef04);

    nkmd_addr = 32'h0000d000;
    nkmd_we = 1'b1;
    #(TCLK);
    nkmd_we = 1'b0;

    nkmd_addr = 32'h0000d000;
    #(TCLK);
    $display("expect %h, actual %h : unread_ff peek", 6'h5, uut.unread_ff);
    $display("expect %h, actual %h : unread_ff ram read", 32'h5, nkmd_data_o);

    nkmd_addr = 32'h0000f000;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[0] ram read", 32'h00beef00, nkmd_data_o);
    nkmd_addr = 32'h0000f001;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[1] ram read", 32'h00beef01, nkmd_data_o);
    nkmd_addr = 32'h0000f002;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[2] ram read", 32'h00beef02, nkmd_data_o);
    nkmd_addr = 32'h0000f003;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[3] ram read", 32'h00beef03, nkmd_data_o);
    nkmd_addr = 32'h0000f004;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[4] ram read", 32'h00beef04, nkmd_data_o);

    shift();

    nkmd_addr = 32'h0000f000;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[0] ram read", 32'h00beef01, nkmd_data_o);
    nkmd_addr = 32'h0000f001;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[1] ram read", 32'h00beef02, nkmd_data_o);
    nkmd_addr = 32'h0000f002;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[2] ram read", 32'h00beef03, nkmd_data_o);
    nkmd_addr = 32'h0000f003;
    #(TCLK);
    $display("expect %h, actual %h : ringbuf[3] ram read", 32'h00beef04, nkmd_data_o);

    shift();
    shift();
    shift();
    shift();

    $display("expect %h, actual %h : unread_ff peek", 6'h0, uut.unread_ff);

    for (i = 0; i < 63; i = i + 1) begin
        push_sample(i);
    end
    $display("expect %d, actual %d : unread_ff peek", 6'd63, uut.unread_ff);
    for (i = 0; i < 63-4; i = i + 1) begin
        shift();
    end
    $display("expect %d, actual %d : unread_ff peek", 6'd4, uut.unread_ff);

    for (i = 63; i < 63+4; i = i + 1) begin
        push_sample(i);
    end
    $display("expect %d, actual %d : unread_ff peek", 6'd8, uut.unread_ff);

    for (i = 63-4; i < 63+4; i = i + 1) begin
        nkmd_addr = 32'h0000f000;
        #(TCLK);
        $display("expect %h, actual %h : ringbuf[0] ram read", i, nkmd_data_o);
        shift();
    end

    #(TCLK);
    $finish(2);
end

endmodule
