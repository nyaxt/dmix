module synth(
    input clk,
    input rst,

	input pop_i,
    output ack_o,
	output [23:0] data_o
);

reg [23:0] data_ff;
reg ack_ff;
assign data_o = ack_ff ? data_ff : 0;
assign ack_o = ack_ff;

parameter STEP = 1600;
parameter PHASE = 16'd139;

reg [15:0] clk_counter;
initial clk_counter = 16'd0;
always @(posedge clk) begin
    ack_ff <= 0;

    if(rst || (pop_i && clk_counter == PHASE*4 - 1)) begin
        clk_counter <= 16'd0;
        data_ff <= 24'd0;
    end else if(pop_i) begin
        clk_counter <= clk_counter + 1;

        if(clk_counter < PHASE)
            data_ff <= data_ff + STEP;
        else if (clk_counter < PHASE*3)
            data_ff <= data_ff - STEP;
        else
            data_ff <= data_ff + STEP;

        ack_ff <= 1;
    end
end
		
endmodule
