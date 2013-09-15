module posedge_latch(
    input clk,

    input pulse_i,
    output pulse_o);

reg [1:0] pulse_hist_ff;
always @(posedge clk) begin
    pulse_hist_ff <= {pulse_hist_ff[0], pulse_i};
end

assign pulse_o = (pulse_hist_ff == 2'b01);

endmodule

module conv_pulse(
    input clk_i,
    input clk_o,
    
    input pulse_i,
    output pulse_o);

reg [3:0] pulse_counter;
always @(posedge clk_i) begin
    if(pulse_i) begin
        pulse_counter <= 4'h5;
    end else if(pulse_counter > 0) begin
        pulse_counter <= pulse_counter - 1;
    end
end
wire pulse = pulse_counter > 0;

posedge_latch latch(.clk(clk_o), .pulse_i(pulse), .pulse_o(pulse_o));

endmodule
