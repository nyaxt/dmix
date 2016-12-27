`default_nettype none

module nkmd_debug #(
    parameter NKMDDBG_WIDTH = 16*8
)(
    input wire clk,
    input wire rst,

    output wire [(NKMDDBG_WIDTH-1):0] dbgout_o,
    input wire [(NKMDDBG_WIDTH-1):0] dbgin_i,
    
    input wire [31:0] data_i,
    output wire [31:0] data_o,
    input wire [31:0] addr_i,
    input wire we_i);

wire [1:0] addr_offset = addr_i[1:0];

reg [31:0] data_o_ff;
always @(posedge clk) begin
    if (addr_i[15:4] == 12'hc80)
        data_o_ff <= dbgin_i[(addr_offset * 32) +: 32];
    else
        data_o_ff <= 8'h00;
end
assign data_o = data_o_ff;

integer i;
reg [(NKMDDBG_WIDTH-1):0] dbgout_ff;
always @(posedge clk) begin
    if (rst) begin
        dbgout_ff <= {(NKMDDBG_WIDTH){1'b0}};
    end else if (addr_i[15:4] == 12'hc80 && we_i) begin
        // ISE bug workaround... :(
        for(i = 0; i < 32; i = i + 1)
            dbgout_ff[(addr_offset*32 + i)] <= data_i[i];
    end
end
assign dbgout_o = dbgout_ff;

endmodule
`default_nettype wire
