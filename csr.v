module csr#(
    parameter NUM_CH = 4,
    parameter VOL_WIDTH = NUM_CH*2*16,
    parameter RATE_WIDTH = NUM_CH*4,
    parameter UDATA_WIDTH = NUM_CH*192,
    parameter CDATA_WIDTH = UDATA_WIDTH
)(
    input clk,
    input rst,

    // memory if
    input [11:0] addr_i,

    input ack_i,
    input [7:0] data_i,

    output [7:0] data_o,

    // registers access
    output [(VOL_WIDTH-1):0] vol_o, // addr: 12'h000 ~
    input [(RATE_WIDTH-1):0] rate_i,  // addr: 12'800 ~
    input [(UDATA_WIDTH-1):0] udata_i,  // addr: 12'900 ~
    input [(CDATA_WIDTH-1):0] cdata_i  // addr: 12'a00 ~
    );

reg [7:0] data_o_ff;
reg ack_o_ff;

wire [3:0] addr_tag = addr_i[11:8];
wire [7:0] addr_offset = addr_i[7:0];

reg [(VOL_WIDTH-1):0] vol_ff;
assign vol_o = vol_ff;

integer i;
always @(posedge clk) begin
    if(rst) begin
        vol_ff <= {(NUM_CH*2){16'h00ff}};
    end else if(ack_i) begin
        case(addr_tag)
        4'h0: begin
            // ISE bug workaround... :(
            for(i = 0; i < 8; i = i + 1)
                vol_ff[(addr_offset*8 + i)] <= data_i[i];
        end
        endcase
    end
end

always @(posedge clk) begin
    case(addr_tag)
    4'h0:
        data_o_ff <= vol_ff[(addr_offset*8) +: 8];
    4'h8:
        data_o_ff <= rate_i[(addr_offset*8) +: 8];
    4'h9:
        data_o_ff <= udata_i[(addr_offset*8) +: 8];
    4'ha:
        data_o_ff <= cdata_i[(addr_offset*8) +: 8];
    default:
        data_o_ff <= 0;
    endcase
end
assign data_o = data_o_ff;

endmodule
