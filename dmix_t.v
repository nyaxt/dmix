`timescale 1ns / 1ps

module dmix_t;

// ins
reg rst;
reg rst_dcm;

reg signal;

parameter Tclk112896 = 88.577; // 11.2896Mhz
parameter Tclk245760 = 40.69; // 24.576Mhz
parameter TclkSPDIF = Tclk245760;
parameter Tclk903200 = 11.07;
parameter Tclk983040 = 10.17;
parameter TCLK = Tclk245760;

reg clk112896;
always #(Tclk112896/2) clk112896 = ~clk112896;
reg clk245760;
always #(Tclk245760/2) clk245760 = ~clk245760;
reg clk903200;
always #(Tclk903200/2) clk903200 = ~clk903200;
reg clk983040;
always #(Tclk983040/2) clk983040 = ~clk983040;
initial begin
	clk112896 = 1'b0;
	clk245760 = 1'b0;
	clk903200 = 1'b0;
	clk983040 = 1'b0;
end

dmix_top uut(.clk112896(clk112896), .clk245760_pad(clk245760), .rst(rst), .spdif_i(signal));
// assign uut.clk903200 = clk903200;
// assign uut.clk983040 = clk983040;
assign uut.rst_dcm = rst_dcm;

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
	// $dumpfile("dmix_t.lxt");
	// $dumpvars(0, dmix_t);

	rst = 1'b0;
    rst_dcm = 1'b0;
    signal = 0;

	#(TCLK*300);
	rst_dcm = 1'b1;
	#(TCLK*10);
	rst_dcm = 1'b0;
	#(TCLK*3);
    rst = 1'b1;
    #(TCLK*3);
    rst = 1'b0;

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

	// #(TCLK*32);
	#(1_000_000);
	$finish(2);
end

endmodule
