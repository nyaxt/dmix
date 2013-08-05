`timescale 1ns / 1ps

module dmix_t;

// ins
reg rst;

reg signal;

parameter Tclk245760 = 40.69; // 24.576Mhz
parameter TclkSPDIF = Tclk245760;
parameter Tclk903200 = 11.07;
parameter Tclk983040 = 10.17;
parameter TCLK = Tclk245760;

reg clk245760;
always #(Tclk245760/2) clk245760 = ~clk245760;
reg clk903200;
always #(Tclk903200/2) clk903200 = ~clk903200;
reg clk983040;
always #(Tclk983040/2) clk983040 = ~clk983040;
initial begin
	clk245760 = 1'b0;
	clk903200 = 1'b0;
	clk983040 = 1'b0;
end

dmix_top uut(.clk245760(clk245760), .rst(rst), .spdif_i(signal));
assign uut.clk903200 = clk903200;
assign uut.clk983040 = clk983040;

task recv_rawbit;
    input b;
    begin
        signal = b;
        #(TclkSPDIF);
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

task recv_bmcctl;
    begin
        recv_bmcbit(1);
        recv_bmcbit(1);
        recv_bmcbit(1);
        recv_bmcbit(1);
    end
endtask

task recv_subframe;
    input [23:0] data;
    begin
        recv_bmcbyte(data[23:16]);
        recv_bmcbyte(data[15:8]);
        recv_bmcbyte(data[7:0]);
        recv_bmcctl();
    end
endtask

reg [23:0] counter;
initial begin
	$dumpfile("dmix_t.lxt");
	$dumpvars(0, dmix_t);

	rst = 1'b0;
    signal = 0;

	#(TCLK*3);
	rst = 1'b1;
	#TCLK;
	rst = 1'b0;
	#(TCLK*3);

    counter <= 0;
    recv_B();
    recv_subframe(counter);
    counter = counter + 1;
    recv_W();
    recv_subframe(counter);
    counter = counter + 1;
    repeat(63) begin
        recv_M();
        recv_subframe(counter);
        counter = counter + 1;
        recv_W();
        recv_subframe(counter);
        counter = counter + 1;
    end
    recv_B();
    recv_subframe(counter);
    counter = counter + 1;
    recv_W();
    recv_subframe(counter);
    counter = counter + 1;
    repeat(63) begin
        recv_M();
        recv_subframe(counter);
        counter = counter + 1;
        recv_W();
        recv_subframe(counter);
        counter = counter + 1;
    end

	#(TCLK*32);
	// #(1000_000_00);
	$finish(2);
end

endmodule
