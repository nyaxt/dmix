// Polyphase filter bank for upsampling from 48000.0kHz to 96000.0kHz
// Depth: 32

module rom_firbank_48_96(
    input clk,
    input [4:0] addr,
    output [23:0] data);
reg [23:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
        0: data_ff <= 24'd2922076;
        1: data_ff <= -24'd1346097;
        2: data_ff <= 24'd812886;
        3: data_ff <= -24'd502799;
        4: data_ff <= 24'd296264;
        5: data_ff <= -24'd158834;
        6: data_ff <= 24'd73147;
        7: data_ff <= -24'd25355;
        8: data_ff <= 24'd2934;
        9: data_ff <= 24'd4665;
        10: data_ff <= -24'd5193;
        11: data_ff <= 24'd3423;
        12: data_ff <= -24'd1657;
        13: data_ff <= 24'd585;
        14: data_ff <= -24'd122;
        15: data_ff <= 24'd9;
        16: data_ff <= 24'd7032135;
        17: data_ff <= -24'd911323;
        18: data_ff <= 24'd205908;
        19: data_ff <= 24'd42019;
        20: data_ff <= -24'd132203;
        21: data_ff <= 24'd147096;
        22: data_ff <= -24'd125911;
        23: data_ff <= 24'd91923;
        24: data_ff <= -24'd58775;
        25: data_ff <= 24'd32984;
        26: data_ff <= -24'd16050;
        27: data_ff <= 24'd6589;
        28: data_ff <= -24'd2166;
        29: data_ff <= 24'd508;
        30: data_ff <= -24'd58;
        31: data_ff <= -24'd2;

        default: data_ff <= 0;
    endcase
end
endmodule
