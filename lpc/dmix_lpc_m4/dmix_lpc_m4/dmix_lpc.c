/*
 * Based on LPCOpen example code
 *
 * @par
 * Permission to use, copy, modify, and distribute this software and its
 * documentation is hereby granted, under NXP Semiconductors' and its
 * licensor's relevant copyrights in the software, without fee, provided that it
 * is used in conjunction with NXP Semiconductors microcontrollers.  This
 * copyright, permission, and disclaimer notice must appear in all copies of
 * this code.
 */

#include <stdio.h>
#include <string.h>

#include "../dmix_lpc_m4/libusbdev.h"
#include "../dmix_lpc_m4/lpc_board_nxp_lpclink2_4370/inc/board.h"

/* The size of the packet buffer. */
#define PACKET_BUFFER_SIZE        4096

/* Application defined LUSB interrupt status  */
#define LUSB_DATA_PENDING       _BIT(0)

/* Packet buffer for processing */
static uint8_t g_rxBuff[PACKET_BUFFER_SIZE];

#if 0
/* SSP */
static uint8_t Tx_Buf[BUFFER_SIZE];
static uint8_t Rx_Buf[BUFFER_SIZE];
static uint8_t dmaChSSPTx, dmaChSSPRx;
#endif

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

static const uint32_t M0APP_BASEADDR = 0x14080000;

void M0App_Boot(uint32_t baseaddr) {
	Chip_RGU_TriggerReset(RGU_M0APP_RST);
	Chip_Clock_Enable(CLK_M4_M0APP);
	Chip_CREG_SetM0AppMemMap(baseaddr);
	Chip_RGU_ClearReset(RGU_M0APP_RST);
}

void M0App_TriggerIPI() {
	__DSB();
	__SEV();
}

void MX_CORE_IRQHandler(void) {
	Chip_CREG_ClearM0AppEvent();
}

uint32_t g_ticks = 0;
void SysTick_Handler(void) {
	++ g_ticks;

	if (g_ticks % 1000 == 0) {
		M0App_TriggerIPI();
	}
}

int main(void)
{
	M0App_Boot(M0APP_BASEADDR);

	SystemCoreClockUpdate();
	Board_Init();

	SysTick_Config(SystemCoreClock / 1000); /* set tick to 1ms */

	while (1) {
		__WFI();
	}

#if 0
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
#endif
}
