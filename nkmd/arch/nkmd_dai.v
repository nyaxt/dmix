module nkmd_dai(
    input clk,
    input rst,

    input [23:0] rxfifo_data_i,
    input rxfifo_lrck,
    output rxfifo_pop_o,
    input rxfifo_empty_i,

    input [31:0] data_i,
    output [31:0] data_o,
    input [31:0] addr_i,
    input we_i);

assign data_o = {8'b0, rxfifo_data_i};
assign rxfifo_pop_o = addr_i[15:0] == 16'hf100;

/*
always @(posedge clk) begin
    if (we_i && addr_i[15:0] == 16'hf180) begin
        $display("[[[ dai write %h ]]]", data_i);
    end
end
*/

endmodule
