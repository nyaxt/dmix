module spdif_dai #(
	parameter CLK_PER_BIT = 8,
	parameter CLK_PER_BIT_LOG2 = 3
)(
    input clk,
    input rst,

    input signal,

    output [23:0] data_o,
    output ack_o,
    output locked,
    output lrck);

parameter SAMELVL_SYNC_COUNT = 3 * CLK_PER_BIT/2;
parameter SAMELVL_SYNC_COUNT_LOG2 = 2 + CLK_PER_BIT_LOG2-1;
reg [(SAMELVL_SYNC_COUNT_LOG2-1):0] samelvl_counter;
reg lastlvl;
always @(posedge clk) begin
	lastlvl <= signal;
	if(lastlvl != signal)
		samelvl_counter <= 0;
	else
		samelvl_counter <= samelvl_counter + 1;
end
wire samelvl_sync = (samelvl_counter == SAMELVL_SYNC_COUNT);

reg clk_counter_rst_ff;
reg [(CLK_PER_BIT_LOG2-1-1):0] clk_counter;
always @(posedge clk) begin
	if(clk_counter_rst_ff)
		clk_counter <= 0;
	else
		clk_counter <= clk_counter + 1;
end

wire subbit_ready = (clk_counter == CLK_PER_BIT/2-1);
reg [(CLK_PER_BIT/2-1):0] subbit_high_counter;
always @(posedge clk) begin
	if(subbit_ready)
		subbit_high_counter <= signal; // start gathering count for next subbit
	else
		subbit_high_counter <= subbit_high_counter + signal;
end
wire subbit = (subbit_high_counter >= CLK_PER_BIT/2/2);

reg [5:0] subbit_hist_ff;
always @(posedge clk) begin
	if(subbit_ready)
		subbit_hist_ff <= {subbit_hist_ff[4:0], subbit};
end

reg [5:0] subbit_counter;
always @(posedge clk) begin
	if(clk_counter_rst_ff)
		subbit_counter <= 0;
	else if(subbit_ready)
		subbit_counter <= subbit_counter + 1;
end
wire fullbit_ready = (subbit_counter[0] == 1'b0);
wire synccode_ready = (subbit_counter == 5);
wire audiodata_ready = (subbit_counter == 5+24*2);

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

// phase FSM
parameter PHASE_SAMELVL = 0;
parameter PHASE_SYNCCODE = 1;
parameter PHASE_AUDIODATA = 2;
reg [2:0] phase;

parameter SYNCCODE_B1 = 6'b010111;
parameter SYNCCODE_W1 = 6'b011011;
parameter SYNCCODE_M1 = 6'b011101;
parameter SYNCCODE_B2 = ~SYNCCODE_B1;
parameter SYNCCODE_W2 = ~SYNCCODE_W1;
parameter SYNCCODE_M2 = ~SYNCCODE_M1;
reg lrck_ff;

always @(posedge clk) begin
	if(rst) begin
		phase <= PHASE_SAMELVL;
	end else begin
		// transition regardless of phase
		if(samelvl_sync) begin
			// sync clk_counter at end of samelvl
			clk_counter_rst_ff <= 1;
			phase <= PHASE_SYNCCODE;
		end else
			clk_counter_rst_ff <= 0;

		// phase transition
		case(phase)
		PHASE_SAMELVL: begin
			// NOP
		end
		PHASE_SYNCCODE: begin
			if(synccode_ready) begin
				case(subbit_hist_ff[5:0])
				SYNCCODE_B1, SYNCCODE_B2: begin
					// FIXME: reset subframe counter
					lrck_ff <= 0;
					phase <= PHASE_AUDIODATA;
				end
				SYNCCODE_W1, SYNCCODE_W2: begin
					lrck_ff <= 1;
					phase <= PHASE_AUDIODATA;
				end
				SYNCCODE_M1, SYNCCODE_M2: begin
					lrck_ff <= 0;
					phase <= PHASE_AUDIODATA;
				end
				default:
					$display("unknown synccode");
					phase <= PHASE_SYNCCODE;
				endcase
			end
		end
		PHASE_AUDIODATA: begin
			// FIXME: does this really NEED to be in PHASE_AUDIODATA?
			// audiodata_ready isn't enough???
			if(audiodata_ready) begin
				data_ff <= bit_hist_ff[23:0];
				phase <= PHASE_EXTRADATA;
			end
		end
		default:
			phase <= PHASE_SAMELVL;
		endcase
	end
end

reg [23:0] data_ff;
always @(posedge clk) begin
	if(phase == PHASE_AUDIODATA && audiodata_ready) begin
		data_ff <= bit_hist_ff[23:0];
		ack_ff <= 1;
	end else
		ack_ff <= 0;
end
assign data_o = data_ff;
assign ack_o = ack_ff;
assign lrck = lrck_ff;


endmodule
