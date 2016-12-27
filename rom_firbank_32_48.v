// Polyphase filter bank for upsampling from 32000.0kHz to 48000.0kHz
// Depth: 32

module rom_firbank_32_48(
    input wire clk,
    input wire [5:0] addr,
    output wire [23:0] data);
reg [23:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
        0: data_ff <= 24'hF55B94; // -697452
        1: data_ff <= 24'h0A4478; // 672888
        2: data_ff <= 24'hFE1404; // -125948
        3: data_ff <= 24'hFD27DD; // -186403
        4: data_ff <= 24'h023120; // 143648
        5: data_ff <= 24'h001A34; // 6708
        6: data_ff <= 24'hFF135A; // -60582
        7: data_ff <= 24'h0062C9; // 25289
        8: data_ff <= 24'h0025B8; // 9656
        9: data_ff <= 24'hFFD29C; // -11620
        10: data_ff <= 24'h000730; // 1840
        11: data_ff <= 24'h000771; // 1905
        12: data_ff <= 24'hFFFC96; // -874
        13: data_ff <= 24'hFFFFED; // -19
        14: data_ff <= 24'h000039; // 57
        15: data_ff <= 24'hFFFFFD; // -3
        16: data_ff <= 24'h21AF3A; // 2207546
        17: data_ff <= 24'h02844A; // 164938
        18: data_ff <= 24'hF9134D; // -453811
        19: data_ff <= 24'h027000; // 159744
        20: data_ff <= 24'h019209; // 102921
        21: data_ff <= 24'hFE3718; // -116968
        22: data_ff <= 24'h003947; // 14663
        23: data_ff <= 24'h009481; // 38017
        24: data_ff <= 24'hFFA9E0; // -22048
        25: data_ff <= 24'hFFF4F8; // -2824
        26: data_ff <= 24'h001C13; // 7187
        27: data_ff <= 24'hFFF8B0; // -1872
        28: data_ff <= 24'hFFFD09; // -759
        29: data_ff <= 24'h0001C5; // 453
        30: data_ff <= 24'hFFFFEB; // -21
        31: data_ff <= 24'hFFFFF8; // -8
        32: data_ff <= 24'h508A9C; // 5278364
        33: data_ff <= 24'hEFF89B; // -1050469
        34: data_ff <= 24'h008D60; // 36192
        35: data_ff <= 24'h04948A; // 300170
        36: data_ff <= 24'hFD8AD9; // -161063
        37: data_ff <= 24'hFF5250; // -44464
        38: data_ff <= 24'h0156D9; // 87769
        39: data_ff <= 24'hFFA255; // -23979
        40: data_ff <= 24'hFFAD9C; // -21092
        41: data_ff <= 24'h004204; // 16900
        42: data_ff <= 24'hFFFDA1; // -607
        43: data_ff <= 24'hFFF084; // -3964
        44: data_ff <= 24'h000583; // 1411
        45: data_ff <= 24'h0000DA; // 218
        46: data_ff <= 24'hFFFF43; // -189
        47: data_ff <= 24'h00000C; // 12

        default: data_ff <= 0;
    endcase
end
endmodule
