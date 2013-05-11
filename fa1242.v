module fa1242(
	output ml,
	output md,
	output mc,
	output rstb,
	input clk_s, // should be slower than 5Mhz
	input rst);

parameter IDLE = 0;
parameter RST = 1;
parameter RST_WAIT = 2;
parameter SET_FUNCTION = 3;
parameter SET_FUNCTION_WAIT = 4;

// attn settings
wire [10:0] al = 0;
wire [10:0] ar = 0;
// mode settings
wire [1:0] om = 2'b0; // outlp: L, outlm: -L, outrp: R, outrm: -R
wire [1:0] bit = 2'b10; // 24bit
wire [1:0] zm = 2'b0; // open drain + pull up
wire atc = 2'b0; // attn. common for LR (off)
wire mute = 2'b0; // unmute
wire [1:0] emph = 2'b0; // no de-emphasis

// for reset
reg [4:0] wait_counter;
parameter WAIT_COUNTER_MAX = 5'h1f;

// for mode set
reg [1:0] mode;
reg [4:0] mode_set_counter;

// FSM transition
reg [3:0] state;
initial state = IDLE;
always @(posedge clk_s or rst) begin
	if(rst) begin
		state <= RST;
		wait_counter <= 0;
		mode_set_counter <= 0;
	end else begin
		case(state)
			IDLE: state <= IDLE;
			RST: begin
				state <= RST_WAIT;
				wait_counter <= 0;
			end
			RST_WAIT: begin
				if(wait_counter == WAIT_COUNTER_MAX) begin
					state <= SET_FUNCTION;
					mode <= 2'd0;
					mode_set_counter <= 2'd0;
				end else begin
					wait_counter <= wait_counter + 1;
				end
			end
			SET_FUNCTION: begin
				if(mode_set_counter == 5'h1f) begin
					mode_set_counter <= 0;
					state <= SET_FUNCTION_WAIT;
					wait_counter <= 0;
				end else
					mode_set_counter <= mode_set_counter + 1;
			end
			SET_FUNCTION_WAIT: begin
				if(wait_counter == WAIT_COUNTER_MAX) begin
					if(mode == 2'd2)
						// all done!
						state <= IDLE;
					else begin
						// set next mode vector
						state <= SET_FUNCTION;
						mode <= mode + 1;
						mode_set_counter <= 2'd0;
					end
				end else begin
					wait_counter <= wait_counter + 1;
				end
			end
			default: state <= IDLE;
		endcase
	end
end

assign rstb = (state == RST);
assign mc = mode_set_counter[0];
assign ml = ~((state == SET_FUNCTION_WAIT) && (wait_counter == 2));

function mode_set_data(
	input [3:0] mode_set_idx,
	input [1:0] mode);

	reg [10:0] mode3;
	
	begin
		mode3 = {om, /*rst*/1'b0, bit, zm, atc, mute, emph};

		if(mode_set_idx < 5)
			case(mode_set_idx)
				// res
				4'd00: mode_set_data = 0;
				4'd01: mode_set_data = 0;
				4'd02: mode_set_data = 0;
				// mode
				4'd03: mode_set_data = mode[1];
				4'd04: mode_set_data = mode[0];
			endcase
		else
			case(mode)
				2'd0: mode_set_data = al[10+5-mode_set_idx];
				2'd1: mode_set_data = ar[10+5-mode_set_idx];
				2'd2: mode_set_data = mode3[10+5-mode_set_idx];
			endcase
	end
endfunction

assign md = mode_set_data(mode_set_counter[4:1], mode);

endmodule
