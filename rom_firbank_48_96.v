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
        0: data_ff <= 24'h164B2D; // 1461037
        1: data_ff <= 24'hF5BAE8; // -673048
        2: data_ff <= 24'h0633AB; // 406443
        3: data_ff <= 24'hFC29F9; // -251399
        4: data_ff <= 24'h0242A4; // 148132
        5: data_ff <= 24'hFEC9C7; // -79417
        6: data_ff <= 24'h008EDD; // 36573
        7: data_ff <= 24'hFFCE7B; // -12677
        8: data_ff <= 24'h0005BB; // 1467
        9: data_ff <= 24'h00091C; // 2332
        10: data_ff <= 24'hFFF5DC; // -2596
        11: data_ff <= 24'h0006AF; // 1711
        12: data_ff <= 24'hFFFCC4; // -828
        13: data_ff <= 24'h000124; // 292
        14: data_ff <= 24'hFFFFC3; // -61
        15: data_ff <= 24'h000004; // 4
        16: data_ff <= 24'h35A6A3; // 3516067
        17: data_ff <= 24'hF90C13; // -455661
        18: data_ff <= 24'h01922A; // 102954
        19: data_ff <= 24'h005211; // 21009
        20: data_ff <= 24'hFEFDCB; // -66101
        21: data_ff <= 24'h011F4C; // 73548
        22: data_ff <= 24'hFF0A15; // -62955
        23: data_ff <= 24'h00B389; // 45961
        24: data_ff <= 24'hFF8D35; // -29387
        25: data_ff <= 24'h00406C; // 16492
        26: data_ff <= 24'hFFE0A7; // -8025
        27: data_ff <= 24'h000CDE; // 3294
        28: data_ff <= 24'hFFFBC5; // -1083
        29: data_ff <= 24'h0000FE; // 254
        30: data_ff <= 24'hFFFFE3; // -29
        31: data_ff <= 24'hFFFFFF; // -1

        default: data_ff <= 0;
    endcase
end
endmodule
