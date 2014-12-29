// Polyphase filter bank for upsampling from 32000.0kHz to 48000.0kHz
// Depth: 32

module rom_firbank_32_48(
    input clk,
    input [5:0] addr,
    output [23:0] data);
reg [23:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
        0: data_ff <= -24'd1394905;
        1: data_ff <= 24'd1345776;
        2: data_ff <= -24'd251897;
        3: data_ff <= -24'd372807;
        4: data_ff <= 24'd287297;
        5: data_ff <= 24'd13416;
        6: data_ff <= -24'd121165;
        7: data_ff <= 24'd50578;
        8: data_ff <= 24'd19313;
        9: data_ff <= -24'd23240;
        10: data_ff <= 24'd3680;
        11: data_ff <= 24'd3811;
        12: data_ff <= -24'd1749;
        13: data_ff <= -24'd39;
        14: data_ff <= 24'd114;
        15: data_ff <= -24'd6;
        16: data_ff <= 24'd4415092;
        17: data_ff <= 24'd329877;
        18: data_ff <= -24'd907622;
        19: data_ff <= 24'd319488;
        20: data_ff <= 24'd205842;
        21: data_ff <= -24'd233936;
        22: data_ff <= 24'd29327;
        23: data_ff <= 24'd76034;
        24: data_ff <= -24'd44096;
        25: data_ff <= -24'd5648;
        26: data_ff <= 24'd14375;
        27: data_ff <= -24'd3745;
        28: data_ff <= -24'd1518;
        29: data_ff <= 24'd906;
        30: data_ff <= -24'd43;
        31: data_ff <= -24'd17;
        32: data_ff <= 24'd10556731;
        33: data_ff <= -24'd2100938;
        34: data_ff <= 24'd72384;
        35: data_ff <= 24'd600340;
        36: data_ff <= -24'd322127;
        37: data_ff <= -24'd88929;
        38: data_ff <= 24'd175538;
        39: data_ff <= -24'd47959;
        40: data_ff <= -24'd42185;
        41: data_ff <= 24'd33800;
        42: data_ff <= -24'd1214;
        43: data_ff <= -24'd7929;
        44: data_ff <= 24'd2822;
        45: data_ff <= 24'd437;
        46: data_ff <= -24'd379;
        47: data_ff <= 24'd24;

        default: data_ff <= 0;
    endcase
end
endmodule
