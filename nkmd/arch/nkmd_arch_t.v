`default_nettype none
`timescale 1ns / 1ps
// `define NODUMP

module nkmd_arch_t;

// ins
reg clk;
reg rst;

parameter TCLK = 20.0; // 50MHz
always #(TCLK/2) clk = ~clk;

initial begin
`ifndef NODUMP
    $dumpfile("nkmd_arch_t.lxt");
    $dumpvars(0, nkmd_arch_t);
`endif

    clk = 1'b0;

    rst = 1'b0;
    #(TCLK*6);
    rst = 1'b1;
    #TCLK;
    rst = 1'b0;
    #TCLK;

//`ifndef NODUMP
    //#100_000;
    #(TCLK*50);
    $finish(2);
//`endif
end

/*
always @(posedge clk) begin
    if (uut.cpu_prog_addr_o == 32'hf)
        $finish(2);
end
*/

nkmd_arch uut(
    .clk(clk), .rst(rst),
    .uart_rx(1'b1),
    .dbgin_i(128'h0123456789abcdef_0123456789abcdef));

endmodule
`default_nettype wire
