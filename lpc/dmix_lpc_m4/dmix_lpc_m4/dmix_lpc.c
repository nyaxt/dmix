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

#include "board.h"

static const uint32_t M0APP_BASEADDR = 0x14080000;

void M0App_Boot(uint32_t baseaddr) {
  Chip_RGU_TriggerReset(RGU_M0APP_RST);
  Chip_Clock_Enable(CLK_M4_M0APP);
  Chip_CREG_SetM0AppMemMap(baseaddr);
  Chip_RGU_ClearReset(RGU_M0APP_RST);
}

void M0App_TriggerIPI() {
  __DSB();
  // __SEV();
  LPC_CREG->M4TXEVENT = 0x1;
}

void MX_CORE_IRQHandler(void) { Chip_CREG_ClearM0AppEvent(); }

uint32_t g_ticks = 0;
void SysTick_Handler(void) {
  ++g_ticks;

  if (g_ticks % 1000 == 0) {
    M0App_TriggerIPI();
  }
}

int main(void) {
  M0App_Boot(M0APP_BASEADDR);

  SystemCoreClockUpdate();
  // Board_Init();

  SysTick_Config(SystemCoreClock / 1000); /* set tick to 1ms */

  while (1) {
    __WFI();
  }
}
