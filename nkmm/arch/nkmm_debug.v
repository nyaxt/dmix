module nkmm_debug(
    input clk,
    input rst,

    output [7:0] debug_led,
    input [7:0] switch,
    
    input [`ACCUM_WIDTH-1:0] data_i,
    output [`ACCUM_WIDTH-1:0] data_o,
    input [`ADDR_WIDTH-1:0] addr_i,
    input we_i);

reg [7:0] data_o_ff;
always @(posedge clk) begin
    if (addr_i == 16'hf000)
        data_o_ff <= switch;
    else
        data_o_ff <= 8'h00;
end
assign data_o[`ACCUM_WIDTH-1:8] = 0;
assign data_o[7:0] = data_o_ff;

reg [7:0] debug_led_ff;
always @(posedge clk) begin
    if (rst) begin
        debug_led_ff <= 8'h00;
    end else if (addr_i == 16'hf000 && we_i) begin
        debug_led_ff <= data_i[7:0];
    end
end
assign debug_led = debug_led_ff;

endmodule
