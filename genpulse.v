module genpulse
#(
    parameter WAVELENGTH = 256, // 49.152MHz / 192kHz
    parameter WAVELENGTH_LOG2 = 8
)(
    input clk,
    input rst,

    output pulse_o);

reg [(WAVELENGTH_LOG2-1):0] counter;
wire pulse_condition = counter == (WAVELENGTH-1);
always @(posedge clk) begin
    if (rst | pulse_condition)
        counter <= 0;
    else
        counter <= counter + 1;
end

reg pulse_ff;
always @(posedge clk) begin
    pulse_ff <= pulse_condition;
end
assign pulse_o = pulse_ff;

endmodule
