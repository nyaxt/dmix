module synth(
	output [23:0] data,
	input clk
);

reg [23:0] data_ff;
initial data_ff = 24'd0;

parameter STEP = 16;
parameter PHASE = 16'd13964;

reg [15:0] clk_counter;
initial clk_counter = 16'd0;
always @(posedge clk)
	if(clk_counter == PHASE*4 - 1) begin
		clk_counter <= 16'd0;
		data_ff <= 24'd0;
	end else begin
		clk_counter <= clk_counter + 1;

		if(clk_counter < PHASE)
			data_ff <= data_ff + STEP;
		else if (clk_counter < PHASE*3)
			data_ff <= data_ff - STEP;
		else
			data_ff <= data_ff + STEP;
	end

assign data = data_ff;
		
endmodule
