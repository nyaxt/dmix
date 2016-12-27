module nkmd_ram(
    input wire clk,

    input wire [31:0] data_i,
    output wire [31:0] data_o,
    input wire [31:0] addr_i,
    input wire we_i);

reg [31:0] ram [1023:0];

reg [31:0] out_ff;
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
