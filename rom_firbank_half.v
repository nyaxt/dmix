// polyphase FIR bank depth 32 x 2
module rom_firbank_half(
    input clk,
    input [5:0] addr,
    output [15:0] data);

reg [15:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
        0: data_ff <= -16'd2;
        1: data_ff <= -16'd13;
        2: data_ff <= 16'd49;
        3: data_ff <= -16'd112;
        4: data_ff <= 16'd208;
        5: data_ff <= -16'd335;
        6: data_ff <= 16'd486;
        7: data_ff <= -16'd644;
        8: data_ff <= 16'd784;
        9: data_ff <= -16'd871;
        10: data_ff <= 16'd855;
        11: data_ff <= -16'd668;
        12: data_ff <= 16'd190;
        13: data_ff <= 16'd863;
        14: data_ff <= -16'd3638;
        15: data_ff <= 16'd27489;
        16: data_ff <= 16'd11503;
        17: data_ff <= -16'd5489;
        18: data_ff <= 16'd3533;
        19: data_ff <= -16'd2399;
        20: data_ff <= 16'd1600;
        21: data_ff <= -16'd1003;
        22: data_ff <= 16'd559;
        23: data_ff <= -16'd243;
        24: data_ff <= 16'd37;
        25: data_ff <= 16'd81;
        26: data_ff <= -16'd132;
        27: data_ff <= 16'd137;
        28: data_ff <= -16'd115;
        29: data_ff <= 16'd82;
        30: data_ff <= -16'd49;
        31: data_ff <= 16'd23;
        32: data_ff <= 16'd23;
        33: data_ff <= -16'd49;
        34: data_ff <= 16'd82;
        35: data_ff <= -16'd115;
        36: data_ff <= 16'd137;
        37: data_ff <= -16'd132;
        38: data_ff <= 16'd81;
        39: data_ff <= 16'd37;
        40: data_ff <= -16'd243;
        41: data_ff <= 16'd559;
        42: data_ff <= -16'd1003;
        43: data_ff <= 16'd1600;
        44: data_ff <= -16'd2399;
        45: data_ff <= 16'd3533;
        46: data_ff <= -16'd5489;
        47: data_ff <= 16'd11503;
        48: data_ff <= 16'd27489;
        49: data_ff <= -16'd3638;
        50: data_ff <= 16'd863;
        51: data_ff <= 16'd190;
        52: data_ff <= -16'd668;
        53: data_ff <= 16'd855;
        54: data_ff <= -16'd871;
        55: data_ff <= 16'd784;
        56: data_ff <= -16'd644;
        57: data_ff <= 16'd486;
        58: data_ff <= -16'd335;
        59: data_ff <= 16'd208;
        60: data_ff <= -16'd112;
        61: data_ff <= 16'd49;
        62: data_ff <= -16'd13;
        63: data_ff <= -16'd2;
    endcase
end
endmodule
