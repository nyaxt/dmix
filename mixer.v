module mixer #(
parameter NUM_CH = 1,
parameter FS = 128
)(
    input clk, // 24.576Mhz
    input rst,

    input [(NUM_CH*2*24-1):0] data_i,
    input [(NUM_CH*2*16-1):0] vol_i,
    output [(NUM_CH*2-1):0] pop_o,

    output [23:0] data_o,
    output lrck_o,
    output we_o
    );

parameter FS2 = FS/2;
parameter OUTPUT_LCH = FS2-1;
parameter OUTPUT_RCH = FS-1;

reg [6:0] phase_counter;

always @(posedge clk) begin
    if(rst) begin
        phase_counter <= 0; 
    end else begin
        if(phase_counter == FS-1)
            phase_counter <= 0;
        else
            phase_counter <= phase_counter + 1;
    end
end

// PIPELINE STAGE 1: mux data/vol
wire [23:0] data_array [(NUM_CH*2-1):0];
wire [15:0] vol_array [(NUM_CH*2-1):0];

genvar ig;
generate
for(ig = 0; ig < NUM_CH*2; ig = ig + 1) begin:g
    assign data_array[ig] = data_i[(24*(ig+1)-1):(24*ig)];
    assign vol_array[ig] = data_i[(16*(ig+1)-1):(16*ig)];
end
endgenerate

reg [23:0] data_mux_ff;
reg [15:0] vol_mux_ff;

wire [3:0] muxsel = phase_counter & (NUM_CH*2-1); // FIXME: 3

always @(posedge clk) begin
    if(phase_counter < NUM_CH*2) begin
        data_mux_ff <= data_array[muxsel];
        vol_mux_ff <= vol_array[muxsel];
    end else begin
        data_mux_ff <= 0;
        vol_mux_ff <= 0;
    end
end

// -- OUTPUT pop_o
reg [(NUM_CH*2-1):0] pop_ff;
assign pop_o = pop_ff;
always @(posedge clk) begin
    case(phase_counter)
        0: pop_ff <= 1 << 0;
        1: pop_ff <= 1 << 1;
        2: pop_ff <= 1 << 2;
        3: pop_ff <= 1 << 3;
        4: pop_ff <= 1 << 4;
        5: pop_ff <= 1 << 5;
        6: pop_ff <= 1 << 6;
        7: pop_ff <= 1 << 7;
        default: pop_ff <= 0;
    endcase
end

// PIPELINE STAGE 2: multiply
reg [23:0] mul_result_ff;

always @(posedge clk) begin
    if(rst) begin
        // mul_result <= 0;
    end else begin
        mul_result_ff <= (data_mux_ff * vol_mux_ff) >> 16;
    end
end

// PIPELINE STAGE 3: add
reg [23:0] mixed [1:0];
wire mixed_ch = (phase_counter & 1);

always @(posedge clk) begin
    if(phase_counter == 0) begin
        mixed[0] <= 0;
        mixed[1] <= 0;
    end else begin
        mixed[mixed_ch] <= mixed[mixed_ch] + mul_result_ff;
    end
end

// OUTPUT control
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

endmodule

module xxx;

wire [23:0] data0 = 24'h123456;
// mixer a(.data_i({data0, data0}), .vol_i({data0, data0});

wire [47:0] data1 = {data0, data0};
mixer a(.data_i(data1));

endmodule
