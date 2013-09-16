module mixer #(
parameter NUM_CH = 1,
parameter NUM_CH_LOG2 = 1,
parameter FS = 256
)(
    input clk, // 24.576Mhz
    input rst,

    // assumes ack arrives 1clk after pop_o
    output [(NUM_CH*2-1):0] pop_o,
    input [(NUM_CH*24-1):0] data_i,
    input [(NUM_CH*2*16-1):0] vol_i,

    input [1:0] pop_i,
    output [23:0] data_o,
    output [1:0] ack_o);

reg [7:0] phase_counter;
parameter PHASE_END = FS-1;

always @(posedge clk) begin
    if(rst) begin
        phase_counter <= 0; 
    end else begin
        phase_counter <= phase_counter + 1;
    end
end

// PIPELINE STAGE 0: OUTPUT pop_o
reg [(NUM_CH*2-1):0] pop_ff;
assign pop_o = pop_ff;
always @(posedge clk) begin
    if(phase_counter == PHASE_END-1)
        pop_ff <= 1;
    else
        pop_ff <= {pop_ff[(NUM_CH*2-2):0], 1'b0};
end

// PIPELINE STAGE 1: mux data/vol
wire [23:0] data_array [(NUM_CH-1):0];
genvar ig;
generate
for(ig = 0; ig < NUM_CH; ig = ig + 1) begin:gd
    assign data_array[ig] = data_i[(24*(ig+1)-1):(24*ig)];
end
endgenerate

wire [15:0] vol_array [(NUM_CH*2-1):0];
generate
for(ig = 0; ig < NUM_CH*2; ig = ig + 1) begin:gv
    assign vol_array[ig] = vol_i[(16*(ig+1)-1):(16*ig)];
end
endgenerate

reg [23:0] data_mux_ff;
reg [15:0] vol_mux_ff;

wire [(NUM_CH_LOG2-1):0] muxsel = phase_counter & (NUM_CH*2-1);

always @(posedge clk) begin
    if(phase_counter < NUM_CH*2) begin
        data_mux_ff <= data_array[muxsel/2];
        vol_mux_ff <= vol_array[muxsel];
    end else begin
        data_mux_ff <= 0;
        vol_mux_ff <= 0;
    end
end

// PIPELINE STAGE 2-5: multiply
wire [23:0] mul_result;
mpemu_scale mp(.clk(clk), .mpcand_i(data_mux_ff), .scale_i(vol_mux_ff), .mprod_o(mul_result));

// PIPELINE STAGE 6: add
reg [23:0] mac_ff [1:0];
wire mac_ch = (phase_counter - 6) & 1;

always @(posedge clk) begin
    if(phase_counter == 0) begin
        mac_ff[0] <= 0;
        mac_ff[1] <= 0;
    end else begin
        mac_ff[mac_ch] <= mac_ff[mac_ch] + mul_result;
    end
end

// OUTPUT
reg [23:0] data_ff [1:0];
always @(posedge clk) begin
    if(phase_counter == PHASE_END) begin
        data_ff[0] <= mac_ff[0];
        data_ff[1] <= mac_ff[1];
    end
end

reg chsel_ff;
reg [1:0] ack_ff;
assign data_o = data_ff[chsel_ff];
assign ack_o = ack_ff;
always @(posedge clk) begin
    ack_ff <= 2'b00;

    if(pop_i[0]) begin
        chsel_ff <= 0;
        ack_ff <= 2'b01;
    end else if(pop_i[1]) begin
        chsel_ff <= 1;
        ack_ff <= 2'b10;
    end
end

endmodule
