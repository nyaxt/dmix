module posedge_latch(
    input clk,

    input wpulse_i,
    output ack_o);

reg wpulse_handled_ff;
// initial wpulse_handled_ff = 1;
reg ack_ff;
// initial ack_ff = 1;
assign ack_o = ack_ff;
always @(posedge clk) begin
    ack_ff <= 0;

    if(wpulse_i && !wpulse_handled_ff) begin
        wpulse_handled_ff <= 1;
        ack_ff <= 1; 
    end else if(!wpulse_i)
        wpulse_handled_ff <= 0;
end

endmodule

module conv_pulse(
    input clk_in,
    input clk_out,
    
    input pulse_in,
    output pulse_out);

reg [3:0] wpulse_counter;
always @(posedge clk_in) begin
    if(pulse_in) begin
        wpulse_counter <= 4'hf;
    end else if(wpulse_counter > 0) begin
        wpulse_counter <= wpulse_counter - 1;
    end
end
wire wpulse = wpulse_counter > 0;

posedge_latch latch(.clk(clk_out), .wpulse_i(wpulse), .ack_o(pulse_out));

endmodule