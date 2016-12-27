`timescale 1ns / 1ps

module dmix_t;

// ins
reg rst;

reg clk;
parameter TCLK = 40;
initial clk = 0;
always #(TCLK/2) clk = ~clk;

reg signal;

reg sck;
parameter TCLK_SCK = 80;
reg mosi;
reg ss;

parameter TclkSPDIF = 40; // 24.576MHz == 192Khz * 32 bit * 2 (biphase)
nkmdhpa uut(
    .rst(rst),
    .clk245760_pad(clk),

    .spdif_i(signal),

    .csr_sck(sck),
    .csr_mosi(mosi),
    .csr_ss(ss));

task recv_rawbit;
    input wire b;
    begin
        signal = b;
        #(TclkSPDIF);//*6);
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
    input wire b;
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
    input wire [7:0] byte;
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
    input wire [23:0] data;
    begin
        recv_bmcbyte(data[7:0]);
        recv_bmcbyte(data[15:8]);
        recv_bmcbyte(data[23:16]);
        recv_bmcctl();
    end
endtask

task spi_cycle;
    input wire [7:0] data;
    begin
        $display("spi tx cycle: %x", data);
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
    end
endtask

`define USE_CAPTURE

`define PROGCMD_LEN 103
reg [7:0] progcmd [(`PROGCMD_LEN-1):0];
initial $readmemh("progcmd.memh", progcmd);

reg replay_capture;
initial replay_capture = 1'b0;
integer i;

reg [22:0] counter;
initial begin
	$dumpfile("nkmdhpa_t.lxt");
	$dumpvars(0, uut);

	rst = 1'b0;
    signal = 0;
    counter = 0;

    mosi = 1'b0;
    ss = 1'b1;

	#(100);
	rst = 1'b1;
	#(200);
	rst = 1'b0;
	#(1500);

    ss = 0;
    for (i = 0; i < (`PROGCMD_LEN-1); i = i + 1) begin
        spi_cycle(progcmd[i]);
    end
    ss = 1;

    #(100);

    $display("--- NKMD dbgin");
    #(TCLK*3);
    ss = 0;
    spi_cycle({4'b1_0_00, 4'h6});
    spi_cycle(8'h00); // offset
    spi_cycle(8'h01);
    spi_cycle(8'h02);
    spi_cycle(8'h03);
    spi_cycle(8'h04);
    spi_cycle(8'h05);
    spi_cycle(8'h06);
    spi_cycle(8'h07);
    spi_cycle(8'h08);
    ss = 1;
    #(TCLK*3);
    $display("--- NKMD rst => 1");
    #(TCLK*3);

    $display("--- NKMD rst => 0");
    #(TCLK*3);
    ss = 0;
    spi_cycle({4'b1_0_00, 4'h4});
    spi_cycle(8'h00); // offset
    spi_cycle(8'h00);
    ss = 1;
    #(TCLK*3);

    replay_capture = 1'b1;
    #(30000);
    $finish(2);
end

`ifndef USE_CAPTURE
always begin
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
    $finish(2);

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

    if (counter > 512)
        $finish(2);
end
`else
reg [31:0] capture [262143:0];
integer capture_iter;
initial $readmemh("spdif_capture3", capture);
initial capture_iter = 0;
always begin
    if (replay_capture) begin
        signal = capture[capture_iter][2];
        capture_iter = capture_iter + 1;
        if (capture_iter > 262143)
            $finish(2);
    end
    #(5);
end
`endif

endmodule
