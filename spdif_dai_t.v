`timescale 1ns / 1ps

module spdif_dai_t;

// ins
reg clk;
reg rst;

reg signal;

spdif_dai uut(
	.clk(clk), .rst(rst), .signal(signal)
);

parameter TCLK_SPDIF = 40.69; // 24.576Mhz
parameter TCLK = TCLK_SPDIF / 2;

task recv_rawbit;
    input b;
    begin
        signal = b;
        #(TCLK_SPDIF);
    end
endtask

task recv_B;
    begin
        recv_rawbit(1);
        recv_rawbit(1);
        recv_rawbit(1);
        recv_rawbit(0);
        recv_rawbit(1);
        recv_rawbit(0);
        recv_rawbit(0);
        recv_rawbit(0);
    end
endtask

initial begin
	$dumpfile("spdif_dai_t.lxt");
	$dumpvars(0, spdif_dai_t);
	
	clk = 1'b0;
	rst = 1'b0;
    signal = 0;

	#(TCLK*3);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#(TCLK*3);

    recv_B();

	#(TCLK*100000);
	// #(1000_000_00);
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
