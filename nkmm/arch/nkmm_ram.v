`include "../nkmm_const.v"

module nkmm_ram(
    input clk,

    input [`ACCUM_WIDTH-1:0] data_i,
    output [`ACCUM_WIDTH-1:0] data_o,
    input [`ADDR_WIDTH-1:0] addr_i,
    input we_i);

reg [`ACCUM_WIDTH-1:0] ram [1023:0];

reg [`ACCUM_WIDTH-1:0] out_ff;
always @(posedge clk) begin
    if (addr_i < 1024) begin
        if (we_i)
            ram[addr_i] <= data_i;

        out_ff <= ram[addr_i];
    end else begin
        out_ff <= 0;
    end
end
assign data_o = out_ff;

endmodule
