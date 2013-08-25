module spdif_dai #(
    parameter MAX_CLK_PER_HALFBIT_LOG2 = 5 // 32 max
)(
    input clk,
    input rst,

    input [(MAX_CLK_PER_HALFBIT_LOG2-1):0] clk_per_halfbit,
    input signal_i,

    output [23:0] data_o,
    output ack_o,
    output locked_o,
    output lrck_o,
    output [191:0] udata_o,
    output [191:0] cdata_o);

parameter HIST_LEN = 4;
reg [(HIST_LEN-1):0] lvl_history_ff;
always @(posedge clk)
    lvl_history_ff <= {lvl_history_ff[(HIST_LEN-2):0], signal_i};

wire lvl_change = lvl_history_ff[3:0] == 4'b1100 || lvl_history_ff[3:0] == 4'b0011;

reg [(MAX_CLK_PER_HALFBIT_LOG2-1):0] clk_counter;
always @(posedge clk) begin
    if(subbit_ready)
        clk_counter <= 0;
    else
        clk_counter <= clk_counter + 1;
end
wire subbit_ready = (clk_counter == clk_per_halfbit) || lvl_change;

wire subbit_needle = lvl_history_ff[1];
reg [(MAX_CLK_PER_HALFBIT_LOG2-1):0] subbit_high_counter;
always @(posedge clk) begin
    if(subbit_ready)
        subbit_high_counter <= subbit_needle; // start gathering count for next subbit
    else
        subbit_high_counter <= subbit_high_counter + subbit_needle;
end
wire subbit = (subbit_high_counter >= clk_per_halfbit/2);

reg [7:0] subbit_hist_ff;
always @(posedge clk) begin
    if(subbit_ready)
        subbit_hist_ff <= {subbit_hist_ff[7:0], subbit};
end
wire [7:0] synccode = subbit_hist_ff;

wire subbit_counter_rst;
reg [5:0] subbit_counter;
parameter SUBBIT_COUNTER_UNLOCKED = 6'h3f;
always @(posedge clk) begin
    if(subbit_counter_rst)
        subbit_counter <= 0;
    else if(subbit_ready && subbit_counter != SUBBIT_COUNTER_UNLOCKED)
        subbit_counter <= subbit_counter + 1;
end

wire fullbit_signal = (subbit_counter[0] == 1'b1);
reg fullbit_signal_prev;
always @(posedge clk) begin
    fullbit_signal_prev <= fullbit_signal;
end
wire fullbit_ready = fullbit_signal && !fullbit_signal_prev;

reg bmcdecode_bit_reg;
always @(subbit_hist_ff[1:0]) begin
    case(subbit_hist_ff[1:0])
    2'b10, 2'b01: 
        bmcdecode_bit_reg = 1;
    2'b11, 2'b00:
        bmcdecode_bit_reg = 0;
    endcase
end

reg [23:0] bit_hist_ff;
always @(posedge clk) begin
    if(fullbit_ready) begin
           bit_hist_ff <= {bmcdecode_bit_reg, bit_hist_ff[23:1]};
    end
end

// sync using synccode
parameter SYNCCODE_B1 = 8'b00010111;
parameter SYNCCODE_W1 = 8'b00011011;
parameter SYNCCODE_M1 = 8'b00011101;
parameter SYNCCODE_B2 = ~SYNCCODE_B1;
parameter SYNCCODE_W2 = ~SYNCCODE_W1;
parameter SYNCCODE_M2 = ~SYNCCODE_M1;
reg startframe_ff;
reg subbit_counter_rst_ff;
reg lrck_ff;

always @(posedge clk) begin
    startframe_ff <= 0;
    subbit_counter_rst_ff <= 0;

    if(rst) begin
        subbit_counter_rst_ff <= 1;
    end else if(subbit_ready) begin
        case(synccode)
        SYNCCODE_B1, SYNCCODE_B2: begin
            startframe_ff <= 1;
            lrck_ff <= 0;
            subbit_counter_rst_ff <= 1;
        end
        SYNCCODE_W1, SYNCCODE_W2: begin
            lrck_ff <= 1;
            subbit_counter_rst_ff <= 1;
        end
        SYNCCODE_M1, SYNCCODE_M2: begin
            lrck_ff <= 0;
            subbit_counter_rst_ff <= 1;
        end
        // default: begin end
        endcase
    end
end
assign subbit_counter_rst = subbit_counter_rst_ff;

// output locked status / lrck
reg [2:0] unlock_tolerance_counter;
parameter UNLOCK_TOLERANCE = 6;
always @(posedge clk) begin
    if(subbit_counter != SUBBIT_COUNTER_UNLOCKED)
        unlock_tolerance_counter <= 0;
    else if (unlock_tolerance_counter != UNLOCK_TOLERANCE)
        unlock_tolerance_counter <= unlock_tolerance_counter + 1;
    end
assign locked_o = (unlock_tolerance_counter != UNLOCK_TOLERANCE);
assign lrck_o = lrck_ff;

// output data
wire audiodata_ready = (subbit_counter == 24*2) && subbit_ready; // subbit_ready is for 1clk pulse width and pipeline wait
reg [23:0] data_ff;
reg ack_ff;
always @(posedge clk) begin
    if(audiodata_ready) begin
        data_ff <= bit_hist_ff[23:0];
        ack_ff <= locked_o; // only ack if locked
    end else
        ack_ff <= 0;
end
assign data_o = data_ff;
assign ack_o = ack_ff;

// output {u,c}data
wire extradata_ready = (subbit_counter == (24+4)*2) && subbit_ready; // subbit_ready is for 1clk pulse width and pipeline wait
reg [191:0] udata_shiftreg;
reg [191:0] cdata_shiftreg;
always @(posedge clk) begin
    if(rst) begin
        udata_shiftreg <= 0;
        cdata_shiftreg <= 0;
    end else if(extradata_ready) begin
        udata_shiftreg <= {udata_shiftreg[190:0], bit_hist_ff[22]};
        cdata_shiftreg <= {cdata_shiftreg[190:0], bit_hist_ff[21]};
    end
end
reg [191:0] udata_ff;
reg [191:0] cdata_ff;
always @(posedge clk) begin
    if(startframe_ff) begin
        udata_ff <= udata_shiftreg;
        cdata_ff <= cdata_shiftreg;
    end
end
assign udata_o = udata_ff;
assign cdata_o = cdata_ff;

endmodule
