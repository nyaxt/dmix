module spdif_dai #(
	parameter CLK_PER_BIT = 8,
	parameter CLK_PER_BIT_LOG2 = 3
)(
    input clk,
    input rst,

    input signal_i,

    output [23:0] data_o,
    output ack_o,
    output locked_o,
    output lrck_o,
    output [191:0] udata_o,
    output [191:0] cdata_o);

parameter SAMELVL_SYNC_COUNT = 3 * CLK_PER_BIT/2;
parameter SAMELVL_SYNC_COUNT_LOG2 = 2 + CLK_PER_BIT_LOG2-1;
reg [(SAMELVL_SYNC_COUNT_LOG2-1):0] samelvl_counter;
reg lastlvl;
always @(posedge clk) begin
	lastlvl <= signal_i;
	if(lastlvl != signal_i)
		samelvl_counter <= 0;
	else
		samelvl_counter <= samelvl_counter + 1;
end
wire samelvl_sync = (samelvl_counter == SAMELVL_SYNC_COUNT-1);

wire clk_counter_rst;
reg [(CLK_PER_BIT_LOG2-1-1):0] clk_counter;
always @(posedge clk) begin
	if(clk_counter_rst)
		clk_counter <= 0;
	else
		clk_counter <= clk_counter + 1;
end

wire subbit_ready = (clk_counter == CLK_PER_BIT/2-1);
reg [(CLK_PER_BIT/2-1):0] subbit_high_counter;
always @(posedge clk) begin
	if(subbit_ready || samelvl_sync)
		subbit_high_counter <= signal_i; // start gathering count for next subbit
	else
		subbit_high_counter <= subbit_high_counter + signal_i;
end
wire subbit = (subbit_high_counter >= CLK_PER_BIT/2/2);

reg [5:0] subbit_hist_ff;
always @(posedge clk) begin
	if(subbit_ready)
		subbit_hist_ff <= {subbit_hist_ff[4:0], subbit};
    else if(!subbit_ready && samelvl_sync)
		subbit_hist_ff <= {subbit_hist_ff[4:0], lastlvl};
end

reg [5:0] subbit_counter;
always @(posedge clk) begin
	if(clk_counter_rst)
		subbit_counter <= 0;
	else if(subbit_ready)
		subbit_counter <= subbit_counter + 1;
end
wire fullbit_ready = (subbit_counter[0] == 1'b0) && (clk_counter == 0);

wire [5:0] synccode = subbit_hist_ff;
wire synccode_ready = (subbit_counter == 5);

reg bmcdecode_bit_reg;
always @(subbit_hist_ff[2:0]) begin
	case(subbit_hist_ff[2:0])
	3'b010, 3'b101: 
		bmcdecode_bit_reg = 1;
	3'b011, 3'b100:
		bmcdecode_bit_reg = 0;
	endcase
end

reg [23:0] bit_hist_ff;
always @(posedge clk) begin
	if(fullbit_ready)
		bit_hist_ff <= {bit_hist_ff[22:0], bmcdecode_bit_reg};
end

// sync using synccode
parameter SYNCCODE_B1 = 6'b010111;
parameter SYNCCODE_W1 = 6'b011011;
parameter SYNCCODE_M1 = 6'b011101;
parameter SYNCCODE_B2 = ~SYNCCODE_B1;
parameter SYNCCODE_W2 = ~SYNCCODE_W1;
parameter SYNCCODE_M2 = ~SYNCCODE_M1;
reg startframe_ff;
reg locked_ff;
reg clk_counter_rst_ff;
reg lrck_ff;

always @(posedge clk) begin
    startframe_ff <= 0;

    if(rst) begin
        locked_ff <= 0;

        clk_counter_rst_ff <= 1;
    end else if(samelvl_sync) begin
        // sync clk_counter at end of samelvl
        clk_counter_rst_ff <= 0;
    end else if(synccode_ready) begin
        case(synccode)
        SYNCCODE_B1, SYNCCODE_B2: begin
            locked_ff <= 1;
            startframe_ff <= 1;
            lrck_ff <= 0;
        end
        SYNCCODE_W1, SYNCCODE_W2: begin
            locked_ff <= 1;
            lrck_ff <= 1;
        end
        SYNCCODE_M1, SYNCCODE_M2: begin
            locked_ff <= 1;
            lrck_ff <= 0;
        end
        default: begin
            // $display("unknown synccode");
            locked_ff <= 0;
            clk_counter_rst_ff <= 1;
        end
        endcase
    end
end
assign clk_counter_rst = clk_counter_rst_ff;

// output locked status / lrck
assign locked_o = locked_ff;
assign lrck_o = lrck_ff;

// output data
wire audiodata_ready = (subbit_counter == 5+24*2+1) && subbit_ready; // subbit_ready is for 1clk pulse width and pipeline wait
reg [23:0] data_ff;
reg ack_ff;
always @(posedge clk) begin
	if(audiodata_ready) begin
		data_ff <= bit_hist_ff[23:0];
		ack_ff <= 1;
	end else
		ack_ff <= 0;
end
assign data_o = data_ff;
assign ack_o = ack_ff;

// output {u,c}data
wire extradata_ready = (subbit_counter == 0) && subbit_ready; // subbit_ready is for 1clk pulse width and pipeline wait
reg [191:0] udata_shiftreg;
reg [191:0] cdata_shiftreg;
always @(posedge clk) begin
	if(extradata_ready) begin
        udata_shiftreg <= {udata_shiftreg[190:0], bit_hist_ff[2]};
        cdata_shiftreg <= {cdata_shiftreg[190:0], bit_hist_ff[1]};
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
