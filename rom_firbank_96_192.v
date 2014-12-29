
module rom_firbank_441_480(
    input clk,
    input [3:0] addr,
    output [23:0] data);
reg [23:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
        0: data_ff <= 24'd2976195;
        1: data_ff <= 24'd661728;
        2: data_ff <= -24'd337284;
        3: data_ff <= -24'd132755;
        4: data_ff <= 24'd27366;
        5: data_ff <= 24'd10170;
        6: data_ff <= -24'd380;
        7: data_ff <= -24'd20;
        8: data_ff <= 24'd3740453;
        9: data_ff <= 24'd1791426;
        10: data_ff <= -24'd71138;
        11: data_ff <= -24'd287085;
        12: data_ff <= -24'd15880;
        13: data_ff <= 24'd24271;
        14: data_ff <= 24'd1758;
        15: data_ff <= -24'd218;

        default: data_ff <= 0;
    endcase
end
endmodule
