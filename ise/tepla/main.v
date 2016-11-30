`timescale 1ns / 1ps

module main(
    input CLK100M,
    input CLK24M576,
    input CLK22M5792,
    output LED_M,
    output [3:0] LED,
    output [3:0] SPDIF,
    output [3:0] GPIO,
    output UART_RXD,
    output UART_TXD,
    output SPI_MISO,
    output SPI_MOSI,
    output SPI_SSEL,
    output SPI_SCK,
    output I2S0_MCLK,
    output I2S0_WS,
    output I2S0_SDA,
    output I2S0_SCK,
    output I2S1_MCLK_N,
    output I2S1_MCLK_P,
    output I2S1_WS,
    output I2S1_SDA,
    output I2S1_SCK,
    output I2S2_MCLK,
    output I2S2_WS,
    output I2S2_SDA,
    output I2S2_SCK,
    output SATA_RX_N,
    output SATA_RX_P,
    output SATA_TX_N,
    output SATA_TX_P
	 );

wire clk = CLK22M5792;

reg [31:0] counter_ff;
always @(posedge clk) begin
    counter_ff <= counter_ff + 4;
end

assign LED_M = counter_ff[28];
assign LED[3:0] = counter_ff[27:24];
assign SPDIF[3:0] = counter_ff[19:16];
assign GPIO[3:0] = counter_ff[19:16];

assign UART_RXD = counter_ff[15];
assign UART_TXD = counter_ff[14];
assign SPI_MISO = counter_ff[16];
assign SPI_MOSI = counter_ff[17];
assign SPI_SSEL = counter_ff[18];
assign SPI_SCK  = counter_ff[19];
assign I2S0_MCLK = counter_ff[16];
assign I2S0_WS = counter_ff[17];
assign I2S0_SDA = counter_ff[18];
assign I2S0_SCK = counter_ff[19];
assign I2S1_MCLK_N = counter_ff[15];
assign I2S1_MCLK_P = counter_ff[16];
assign I2S1_WS = counter_ff[17];
assign I2S1_SDA = counter_ff[18];
assign I2S1_SCK = counter_ff[19];
assign I2S2_MCLK = counter_ff[16];
assign I2S2_WS   = counter_ff[17];
assign I2S2_SDA  = counter_ff[18];
assign I2S2_SCK  = counter_ff[19];
assign SATA_RX_N = counter_ff[16];
assign SATA_RX_P = counter_ff[17];
assign SATA_TX_N = counter_ff[18];
assign SATA_TX_P = counter_ff[19];

endmodule
