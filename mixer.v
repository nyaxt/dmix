// `define DEBUG

module mixer #(
    parameter NUM_CH_IN = 8, // must be multiple of NUM_CH_OUT
    parameter NUM_CH_IN_LOG2 = 3,

    parameter NUM_CH_OUT = 2,
    parameter NUM_CH_OUT_LOG2 = 1,

    parameter VOL_WIDTH = 32
)(
    input clk, // 49.152Mhz
    input rst,

    input [(NUM_CH_IN-1):0] rst_ch,
    output [(NUM_CH_IN-1):0] pop_o, // optional: ack_i accepted any time
    input [(NUM_CH_IN-1):0] ack_i,
    input [(NUM_CH_IN*24-1):0] data_i,

    input [(NUM_CH_IN*VOL_WIDTH-1):0] vol_i,

    input [(NUM_CH_OUT-1):0] pop_i,
    output [23:0] data_o,
    output [(NUM_CH_OUT-1):0] ack_o);

parameter MULT_LATENCY = 6;

// Input ringbuf
wire [23:0] buffered_data [(NUM_CH_IN-1):0];
genvar igi;
generate
for (igi = 0; igi < NUM_CH_IN; igi = igi + 1) begin:gi
    ringbuf #(.LEN(4), .LEN_LOG2(2)) rb(
        .clk(clk), .rst(rst | rst_ch[igi]),
        .data_i(data_i[(igi*24) +: 24]), .we_i(ack_i[igi]),
        .pop_i(pop_o[igi]), .offset_i(0), .data_o(buffered_data[igi]));
end
endgenerate

// Sequencer
// OUTPUT:
parameter TIMESLICE = NUM_CH_IN/NUM_CH_OUT + MULT_LATENCY + 1 + 1; // saturate 1clk, sum 1clk
parameter TIMESLICE_LOG2 = NUM_CH_IN_LOG2-NUM_CH_OUT_LOG2 + 4; // assumes MULT_LATENCY + 1 + 1 < 16
reg [(NUM_CH_IN_LOG2-1):0] processing_in_ch_ff;
reg [(NUM_CH_OUT_LOG2-1):0] processing_out_ch_ff;
reg [(TIMESLICE_LOG2-1):0] timeslice_counter;
wire end_of_cycle = (timeslice_counter == TIMESLICE-1) ? 1'b1 : 1'b0;
always @(posedge clk) begin
    if (rst) begin
        processing_in_ch_ff <= 0;
        processing_out_ch_ff <= 0;
        timeslice_counter <= 0;
    end else if (end_of_cycle) begin
        timeslice_counter <= 0;

        if (processing_out_ch_ff == NUM_CH_OUT-1) begin
            processing_in_ch_ff <= 0;
            processing_out_ch_ff <= 0;
        end else begin
            processing_in_ch_ff <= processing_out_ch_ff + 1;
            processing_out_ch_ff <= processing_out_ch_ff + 1;
        end
    end else begin
        processing_in_ch_ff <= processing_in_ch_ff + NUM_CH_OUT;
        timeslice_counter <= timeslice_counter + 1;
    end
end

// Cycle validity checker
reg cycle_valid_ff;

// - pop_i latch
reg [(NUM_CH_OUT-1):0] ack_pop_ff;
wire [(NUM_CH_OUT-1):0] pop_i_latched;
genvar igo;
generate
for (igo = 0; igo < NUM_CH_OUT; igo = igo + 1) begin:go
    pop_latch pop_latch(
        .clk(clk), .rst(rst),
        .pop_i(pop_i[igo]),
        .ack_pop_i(ack_pop_ff[igo]), .pop_latched_o(pop_i_latched[igo]));
end
endgenerate

// - cycle validity check
always @(posedge clk) begin
    ack_pop_ff <= 0;

    if (rst) begin
        cycle_valid_ff <= 1'b0;
    end else if (timeslice_counter == 0) begin
        ack_pop_ff <= 1 << processing_out_ch_ff;
    end else if (timeslice_counter == 1) begin
        cycle_valid_ff <= pop_i_latched[processing_out_ch_ff];
    end
end

// Supply mpcand
// OUTPUT:
wire [23:0] mpcand = buffered_data[processing_in_ch_ff];

// Supply scale
wire [(VOL_WIDTH-1):0] scale = 32'h01_000000; //vol_i[(processing_in_ch_ff*VOL_WIDTH) +: VOL_WIDTH];

// Multiplier
// OUTPUT:
wire [31:0] mprod;
mpemu_scale mp(
    .clk(clk),
    .mpcand_i(mpcand), .scale_i(scale),
    .mprod_o(mprod));

// Satulated product
reg [23:0] saturated_mprod_ff;
always @(posedge clk) begin
    if (mprod[31] == 1'b0) begin
        // sum +
        if (mprod[30:23] == 8'b1111_1111)
            saturated_mprod_ff <= 24'h7f_ffff; // overflow
        else
            saturated_mprod_ff <= {1'b0, mprod[22:0]};
    end else begin
        // sum -
        if (mprod[30:23] == 8'b0000_0000)
            saturated_mprod_ff <= 24'h80_0000; // underflow
        else
            saturated_mprod_ff <= {1'b1, mprod[22:0]};
    end
end

// - drop first MULT_LATENCY + 1 saturated products
reg [(MULT_LATENCY+1-1):0] kill_result_ff;
always @(posedge clk) begin
    if (end_of_cycle)
        kill_result_ff <= 0;
    else
        kill_result_ff <= {1'b1, kill_result_ff[(MULT_LATENCY+1-1):1]};
end
wire product_valid = kill_result_ff[0];

// Adder
reg [23:0] sum_ff;

function [23:0] saturated_add(
    input [23:0] a,
    input [23:0] b);

reg [24:0] aext;
reg [24:0] bext;
reg [24:0] sumext;

begin
    aext = {a[23], a};
    bext = {b[23], b};
    sumext = $signed(aext) + $signed(bext);

    case (sumext[23:22])
        2'b00, 2'b11: // sum is in expressible range
            saturated_add = sumext[23:0];
        2'b01: // overflow
            saturated_add = 32'h7fff_ffff;
        2'b10: // underflow
            saturated_add = 32'h8000_0000;
    endcase
end
endfunction

always @(posedge clk) begin
    if (!product_valid) begin
        sum_ff <= 0;
    end else begin
        `ifdef DEBUG
        if (cycle_valid_ff)
            $display("mixer outch: %d curr_sum: %h. mpcand %h * mplier %h = %h",
                processing_out_ch_ff, $signed(sum_ff), $signed(mp.delayed_a2), $signed(mp.delayed_b2), $signed(saturated_mprod_ff));
        `endif
        sum_ff <= saturated_add(sum_ff, saturated_mprod_ff);
    end
end

// Result
assign data_o = sum_ff;
assign ack_o = (end_of_cycle & cycle_valid_ff) << processing_out_ch_ff;
assign pop_o = {(NUM_CH_IN/NUM_CH_OUT){ack_o}};

endmodule
