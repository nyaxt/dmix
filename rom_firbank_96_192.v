// Polyphase filter bank for upsampling from 96000.0kHz to 192000.0kHz
// Depth: 16

module rom_firbank_96_192(
    input clk,
    input [3:0] addr,
    output [23:0] data);
reg [23:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
        0: data_ff <= 24'h16B4E1; // 1488097
        1: data_ff <= 24'h050C70; // 330864
        2: data_ff <= 24'hFD6D3E; // -168642
        3: data_ff <= 24'hFEFCB7; // -66377
        4: data_ff <= 24'h003573; // 13683
        5: data_ff <= 24'h0013DD; // 5085
        6: data_ff <= 24'hFFFF42; // -190
        7: data_ff <= 24'hFFFFF6; // -10
        8: data_ff <= 24'h1C8992; // 1870226
        9: data_ff <= 24'h0DAAE0; // 895712
        10: data_ff <= 24'hFF750F; // -35569
        11: data_ff <= 24'hFDCF4A; // -143542
        12: data_ff <= 24'hFFE0FC; // -7940
        13: data_ff <= 24'h002F67; // 12135
        14: data_ff <= 24'h00036F; // 879
        15: data_ff <= 24'hFFFF93; // -109

        default: data_ff <= 0;
    endcase
end
endmodule
