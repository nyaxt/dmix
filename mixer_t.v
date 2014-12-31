`timescale 1ns / 1ps

module mixer_t;

// ins
reg clk;
reg rst;

reg [7:0] ack_i;
reg [(8*24-1):0] data_i;

reg [1:0] pop_i;

mixer uut(
    .clk(clk), .rst(rst), .rst_ch(0),
    .ack_i(ack_i), .data_i(data_i),
    .vol_i({32'h01_000000, 32'h02_000000, 32'h03_000000, 32'h04_000000,
            32'h05_000000, 32'h06_000000, 32'h07_000000, 32'h08_000000}),
    .pop_i(pop_i));

parameter TCLK = 10;
always #(TCLK/2) clk = ~clk;

initial begin
	$dumpfile("mixer_t.lxt");
	$dumpvars(0, uut);

    clk = 0;

    ack_i = 0;
    data_i = 0;
    pop_i = 0;

    rst = 0;
    #(TCLK);
    rst = 1;
    #(TCLK);
    rst = 0;

    #(TCLK * 1000);
	$finish(2);
end

always begin
    #(TCLK*32);
    pop_i = 2'b11;
    #(TCLK);
    pop_i = 2'b00;
end

genvar ig;
generate
for(ig = 0; ig < 8; ig = ig + 1) begin:g
    wire [7:0] ch = ig;
    reg [15:0] counter_ff;
    initial begin
        counter_ff <= 0;
    end

    always @(posedge clk) begin
        if (uut.pop_o[ig]) begin
            #(TCLK);
            data_i[(ig*24) +: 24] = {ch, counter_ff};
            counter_ff = counter_ff + 1;
            ack_i[ig] = 1;
            #(TCLK);
            ack_i[ig] = 0;
        end
    end
end
endgenerate

genvar iga;
generate
for(iga = 0; iga < 2; iga = iga + 1) begin:ga
    always @(posedge clk) begin
        if (uut.ack_o[iga])
            $display("ch: %d. out: %h", iga, uut.data_o);
    end
end
endgenerate

endmodule
