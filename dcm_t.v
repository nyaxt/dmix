`timescale 1ns / 1ps

module dcm_t;

dmix_dcm uut(); 

initial begin
	$dumpfile("dcm_t.lxt");
	$dumpvars(0, dcm_t);
    
    #(250);

	$finish(2);
end

endmodule
