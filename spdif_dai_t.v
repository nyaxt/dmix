`timescale 1ns / 1ps

module spdif_dai_t;

// ins
reg clk;
reg rst;

reg signal;

spdif_dai uut(
	.clk(clk), .rst(rst), .signal_i(signal)
);

parameter TCLK_SPDIF = 40.69; // 24.576Mhz
parameter TCLK = TCLK_SPDIF / 4;

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
    recv_bmcbyte(8'hde);
    recv_bmcbyte(8'had);
    recv_bmcbyte(8'hff);
    recv_bmcctl();

    recv_W();
    recv_bmcbyte(8'h00);
    recv_bmcbyte(8'hbe);
    recv_bmcbyte(8'hef);
    recv_bmcctl();

    recv_M();
    recv_bmcbyte(8'h01);
    recv_bmcbyte(8'h23);
    recv_bmcbyte(8'h45);
    recv_bmcctl();

    recv_W();
    recv_bmcbyte(8'h67);
    recv_bmcbyte(8'h89);
    recv_bmcbyte(8'hab);
    recv_bmcctl();

    counter <= 0;
    recv_B();
    recv_subframe(counter);
    counter = counter + 1;
    recv_W();
    recv_subframe(counter);
    counter = counter + 1;
    repeat(95) begin
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
    repeat(95) begin
        recv_M();
        recv_subframe(counter);
        counter = counter + 1;
        recv_W();
        recv_subframe(counter);
        counter = counter + 1;
    end

	#(TCLK*32);
	$finish(2);
end

always #(TCLK/2) clk = ~clk;

always @(posedge clk) begin
    if(uut.ack_o) begin
        $display("data_o: %x", uut.data_o);
    end
end

endmodule
