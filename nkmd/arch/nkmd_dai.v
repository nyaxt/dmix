module nkmd_dai(
    input clk,
    input rst,

    input [31:0] data_i,
    output [31:0] data_o,
    input [31:0] addr_i,
    input we_i);

reg [23:0] data_o_ff;
always @(posedge clk) begin
    case (addr_i[15:0])
    16'hf100:
        data_o_ff <= 24'habcdef;
    16'hf180:
        data_o_ff <= 24'hcafebb;
    default:
        data_o_ff <= 24'h000000;
    endcase
end
assign data_o = {8'b0, data_o_ff};

always @(posedge clk) begin
    if (we_i && addr_i == 32'hf1800) begin
        $display("[[[ dai write %h ]]]", data_i);
    end
end

endmodule
