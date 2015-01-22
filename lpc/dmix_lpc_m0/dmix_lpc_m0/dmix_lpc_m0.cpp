#include "board.h"
#include "util.h"
#include "usbhandler.h"

#include <cr_section_macros.h>

int* g_toggle = (int*)0x2000c000;
int* g_i = (int*)0x2000c004;
int* g_j = (int*)0x2000c008;

void Dmix_SSP_Init()
{
	Board_SSP_Init(LPC_SSP1);
	Chip_SSP_Init(LPC_SSP1);
	Chip_SSP_SetFormat(LPC_SSP1, SSP_BITS_8, SSP_FRAMEFORMAT_SPI, SSP_CLOCK_MODE0);
	Chip_SSP_Enable(LPC_SSP1);

	Chip_GPDMA_Init(LPC_GPDMA);

	NVIC_DisableIRQ(DMA_IRQn);
	NVIC_DisableIRQ(USB0_IRQn);
	// High prio
	NVIC_SetPriority(DMA_IRQn, 0x01);
	// Low prio
	NVIC_SetPriority(USB0_IRQn, 0x02);
	NVIC_EnableIRQ(DMA_IRQn);

	Chip_SSP_SetMaster(LPC_SSP1, TRUE);
}

#if 0
{
	isDmaTxfCompleted = isDmaRxfCompleted = 0;
	Chip_SSP_DMA_Enable(LPC_SSP1);
	/* data Tx_Buf --> SSP */
	Chip_GPDMA_Transfer(LPC_GPDMA, dmaChSSPTx,
			(uint32_t) &Tx_Buf[0],
			GPDMA_CONN_SSP1_Tx,
			GPDMA_TRANSFERTYPE_M2P_CONTROLLER_DMA,
			BUFFER_SIZE);
	/* data SSP --> Rx_Buf */
	Chip_GPDMA_Transfer(LPC_GPDMA, dmaChSSPRx,
			GPDMA_CONN_SSP1_Rx,
			(uint32_t) &Rx_Buf[0],
			GPDMA_TRANSFERTYPE_P2M_CONTROLLER_DMA,
			BUFFER_SIZE);

	while (!isDmaTxfCompleted || !isDmaRxfCompleted) {}
	Chip_SSP_DMA_Disable(LPC_SSP1);
}
#endif


extern "C" {

void MX_CORE_IRQHandler(void) {
	Chip_CREG_ClearM4Event();

	if (*g_toggle)
		Chip_GPIO_SetValue(LPC_GPIO_PORT, 0, 1<<8);
	else
		Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	*g_toggle = !*g_toggle;
	++ *g_i;
}

}

int main(void) {
    SystemCoreClockUpdate();
    Board_Init();

	NVIC_EnableIRQ(M4_IRQn);

	Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	Dmix_SSP_Init();
	USBHandler::init();

	for(;;) {
		bool didSomething = false;

		if (USBHandler::getInstance()->process())
			didSomething = true;

		if (!didSomething)
			__WFI();
	}

    return 0;
}
