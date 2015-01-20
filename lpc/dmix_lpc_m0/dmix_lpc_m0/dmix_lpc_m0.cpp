#include "board.h"

#include "libusbdev.h"
#include <cr_section_macros.h>

/* The size of the packet buffer. */
#define PACKET_BUFFER_SIZE        4096

/* Application defined LUSB interrupt status  */
#define LUSB_DATA_PENDING       _BIT(0)

/* Packet buffer for processing */
static uint8_t g_rxBuff[PACKET_BUFFER_SIZE];

int* g_toggle = (int*)0x2000c000;
int* g_i = (int*)0x2000c004;
int* g_j = (int*)0x2000c008;

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

int main(void) {
    SystemCoreClockUpdate();
    Board_Init();

	NVIC_EnableIRQ(M4_IRQn);

	Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	Dmix_SSP_Init();
	libusbdev_init(USB_STACK_MEM_BASE, USB_STACK_MEM_SIZE);

	while (1) {
		/* wait until host is connected */
		while (libusbdev_Connected() == 0) {
			/* Sleep until next IRQ happens */
			__WFI();
		}

		while (libusbdev_Connected()) {
			if (libusbdev_QueueReadDone() != -1) {

				/* Dummy process read data ......*/
				/* requeue read request */
				libusbdev_QueueReadReq(g_rxBuff, PACKET_BUFFER_SIZE);
			}

			if (libusbdev_QueueSendDone() == 0) {
				/* Queue send request */
				libusbdev_QueueSendReq(g_rxBuff, PACKET_BUFFER_SIZE);
			}
		}
	}

    return 0;
}
