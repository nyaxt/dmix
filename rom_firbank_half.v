// polyphase FIR bank depth 32 x 2
module rom_firbank_half(
    input [6:0] addr,
    output [15:0] data);

reg [15:0] data_reg;
assign data = data_reg;
always @(addr) begin
    case(addr)
        0: data_reg = -16'd2;
        1: data_reg = -16'd13;
        2: data_reg = 16'd49;
        3: data_reg = -16'd112;
        4: data_reg = 16'd208;
        5: data_reg = -16'd335;
        6: data_reg = 16'd486;
        7: data_reg = -16'd644;
        8: data_reg = 16'd784;
        9: data_reg = -16'd871;
        10: data_reg = 16'd855;
        11: data_reg = -16'd668;
        12: data_reg = 16'd190;
        13: data_reg = 16'd863;
        14: data_reg = -16'd3638;
        15: data_reg = 16'd27489;
        16: data_reg = 16'd11503;
        17: data_reg = -16'd5489;
        18: data_reg = 16'd3533;
        19: data_reg = -16'd2399;
        20: data_reg = 16'd1600;
        21: data_reg = -16'd1003;
        22: data_reg = 16'd559;
        23: data_reg = -16'd243;
        24: data_reg = 16'd37;
        25: data_reg = 16'd81;
        26: data_reg = -16'd132;
        27: data_reg = 16'd137;
        28: data_reg = -16'd115;
        29: data_reg = 16'd82;
        30: data_reg = -16'd49;
        31: data_reg = 16'd23;
        32: data_reg = 16'd23;
        33: data_reg = -16'd49;
        34: data_reg = 16'd82;
        35: data_reg = -16'd115;
        36: data_reg = 16'd137;
        37: data_reg = -16'd132;
        38: data_reg = 16'd81;
        39: data_reg = 16'd37;
        40: data_reg = -16'd243;
        41: data_reg = 16'd559;
        42: data_reg = -16'd1003;
        43: data_reg = 16'd1600;
        44: data_reg = -16'd2399;
        45: data_reg = 16'd3533;
        46: data_reg = -16'd5489;
        47: data_reg = 16'd11503;
        48: data_reg = 16'd27489;
        49: data_reg = -16'd3638;
        50: data_reg = 16'd863;
        51: data_reg = 16'd190;
        52: data_reg = -16'd668;
        53: data_reg = 16'd855;
        54: data_reg = -16'd871;
        55: data_reg = 16'd784;
        56: data_reg = -16'd644;
        57: data_reg = 16'd486;
        58: data_reg = -16'd335;
        59: data_reg = 16'd208;
        60: data_reg = -16'd112;
        61: data_reg = 16'd49;
        62: data_reg = -16'd13;
        63: data_reg = -16'd2;
        default: data_reg = 0;
    endcase
end
endmodule
