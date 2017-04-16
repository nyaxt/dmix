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
#include "spi.h"
#include "usbhandler.h"
#include "util.h"

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

int g_tickSinceLastDidSomething = 0;

void Config_NVIC() {
  NVIC_DisableIRQ(DMA_IRQn);
  NVIC_DisableIRQ(USB0_IRQn);
  // High prio
  NVIC_SetPriority(DMA_IRQn, 0x01);
  // Low prio
  NVIC_SetPriority(USB0_IRQn, 0x02);
}

uint32_t g_ticks = 0;

extern "C" {

void MX_CORE_IRQHandler(void) { Chip_CREG_ClearM0AppEvent(); }

void SysTick_Handler(void) {
  ++g_ticks;

  if (g_ticks % 10 == 0) {
    M0App_TriggerIPI();
  }

  if (g_tickSinceLastDidSomething < 1000)
    g_tickSinceLastDidSomething ++;
}

void DMA_IRQHandler(void) { SPI::getInstance()->onDMAIRQ(); }

} // extern "C"

int main(void) {
  M0App_Boot(M0APP_BASEADDR);

  SystemCoreClockUpdate();
  SysTick_Config(SystemCoreClock / 1000); /* set tick to 1ms */

  SPI::init();
  USBHandler::init();

  for (;;) {
    bool didSomething = false;

    if (USBHandler::getInstance()->process())
      didSomething = true;

    if (SPI::getInstance()->callCallbackIfDone())
      didSomething = true;

    if (didSomething)
      g_tickSinceLastDidSomething = 0;
    else if (g_tickSinceLastDidSomething > 300)
      __WFI();
  }
}