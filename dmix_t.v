`timescale 1ns / 1ps

module dmix_t;

// ins
reg rst;

reg signal;

parameter TclkSPDIF = 40; // 24.576MHz == 192Khz * 32 bit * 2 (biphase)
dmix_top uut(.rst(rst), .spdif_i(signal));

task recv_rawbit;
    input b;
    begin
        signal = b;
        #(TclkSPDIF*2);
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
        recv_bmcbit(byte[0]);
        recv_bmcbit(byte[1]);
        recv_bmcbit(byte[2]);
        recv_bmcbit(byte[3]);
        recv_bmcbit(byte[4]);
        recv_bmcbit(byte[5]);
        recv_bmcbit(byte[6]);
        recv_bmcbit(byte[7]);
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
        recv_bmcbyte(data[7:0]);
        recv_bmcbyte(data[15:8]);
        recv_bmcbyte(data[23:16]);
        recv_bmcctl();
    end
endtask

// `define USE_CAPTURE

initial begin
	$dumpfile("dmix_t.lxt");
	$dumpvars(0, uut);

	rst = 1'b0;
    signal = 0;

	#(10);
	rst = 1'b1;
	#(40);
	rst = 1'b0;
	#(50);
end

`ifndef USE_CAPTURE
reg [22:0] counter;
always begin
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

    if (counter > 1)
        $finish(2);
end
`else
reg [31:0] capture [262143:0];
integer capture_iter;
initial $readmemh("spdif_capture3", capture);
initial capture_iter = 0;
always begin
    signal = capture[capture_iter][2];
    capture_iter = capture_iter + 1;
    if (capture_iter > 262143)
        $finish(2);
    #(5);
end
`endif

endmodule
