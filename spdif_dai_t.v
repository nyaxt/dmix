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
        if(signal) begin
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(1);
        end else begin
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(0);
        end
    end
endtask

task recv_M;
    begin
        if(signal) begin
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(1);
        end else begin
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(0);
        end
    end
endtask

task recv_W;
    begin
        if(signal) begin
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(1);
        end else begin
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(0);
            recv_rawbit(1);
            recv_rawbit(0);
            recv_rawbit(0);
        end
    end
endtask

task recv_bmcbit;
    input b;
    begin
        if(signal) begin
            if(b) begin
                recv_rawbit(0);
                recv_rawbit(1);
            end else begin
                recv_rawbit(0);
                recv_rawbit(0);
            end
        end else begin
            if(b) begin
                recv_rawbit(1);
                recv_rawbit(0);
            end else begin
                recv_rawbit(1);
                recv_rawbit(1);
            end
        end
    end
endtask

task recv_bmcbyte;
    input [7:0] byte;
    begin
        recv_bmcbit(byte[7]);
        recv_bmcbit(byte[6]);
        recv_bmcbit(byte[5]);
        recv_bmcbit(byte[4]);
        recv_bmcbit(byte[3]);
        recv_bmcbit(byte[2]);
        recv_bmcbit(byte[1]);
        recv_bmcbit(byte[0]);
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
    recv_bmcbyte(8'h00);
    recv_bmcbyte(8'h00);
    recv_bmcbyte(8'h00);
    recv_bmcbit(1);
    recv_bmcbit(1);
    recv_bmcbit(1);
    recv_bmcbit(1);

	#(TCLK*100000);
	// #(1000_000_00);
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

endmodule
