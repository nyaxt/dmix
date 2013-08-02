module mixer #(
parameter NUM_CH = 1,
parameter NUM_CH_LOG2 = 1,
parameter FS = 128
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

reg [6:0] phase_counter;

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
    if(phase_counter < NUM_CH*2)
        pop_ff <= 1 << phase_counter;
    else
        pop_ff <= 0;
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

wire [(NUM_CH_LOG2-1):0] muxsel = (phase_counter-1) & (NUM_CH*2-1);

always @(posedge clk) begin
    if(0 < phase_counter && phase_counter < NUM_CH*2+1) begin
        data_mux_ff <= data_array[muxsel/2];
        vol_mux_ff <= vol_array[muxsel];
    end else begin
        data_mux_ff <= 0;
        vol_mux_ff <= 0;
    end
end

// PIPELINE STAGE 2-5: multiply
wire [23:0] mul_result;
mpemu mp(.clk(clk), .mpcand_i(data_mux_ff), .mplier_i(vol_mux_ff), .mprod_o(mul_result));

reg [23:0] mul_result_ff;
always @(posedge clk) begin
    mul_result_ff <= (data_mux_ff * vol_mux_ff) >> 16;
end

// PIPELINE STAGE 6: add
reg [23:0] mixed_ff [1:0];
wire mixed_ch = (phase_counter - 6) & 1;

always @(posedge clk) begin
    if(phase_counter == 0) begin
        mixed_ff[0] <= 0;
        mixed_ff[1] <= 0;
    end else begin
        mixed_ff[mixed_ch] <= mixed_ff[mixed_ch] + mul_result_ff;
    end
end

// OUTPUT
reg chsel_ff;
reg [1:0] ack_ff;
assign data_o = mixed_ff[chsel_ff];
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

/*
// OUTPUT control
parameter FS2 = FS/2;
parameter OUTPUT_LCH = FS2-1;
parameter OUTPUT_RCH = FS-1;

wire phase_output_lch = (phase_counter == OUTPUT_LCH);
wire phase_output_rch = (phase_counter == OUTPUT_RCH);

wire we_o = (phase_output_lch | phase_output_rch);

reg lrck_ff;
reg data_ff;
assign lrck_o = lrck_ff;
assign data_o = data_ff;
always @(posedge clk) begin
    if(rst) begin
        lrck_ff <= 0;
    end else begin
        if(phase_output_lch) begin
            lrck_ff <= 0;
            data_ff <= mixed[0];
        end else if(phase_output_rch) begin
            lrck_ff <= 1;
            data_ff <= mixed[1];
        end
    end
end
*/
