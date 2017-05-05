#include "board.h"
#include "spi.h"
#include "util.h"

#include <cr_section_macros.h>

#if 0
int *g_toggle = (int *)0x2000c000;
int *g_i = (int *)0x2000c004;
int *g_j = (int *)0x2000c008;
#endif
int g_tickSinceLastDidSomething = 0;

void Config_NVIC() {
  NVIC_DisableIRQ(DMA_IRQn);
  NVIC_DisableIRQ(USB0_IRQn);
  // High prio
  NVIC_SetPriority(DMA_IRQn, 0x01);
  // Low prio
  NVIC_SetPriority(USB0_IRQn, 0x02);
}

extern "C" {

void MX_CORE_IRQHandler(void) {
  Chip_CREG_ClearM4Event();

#if 0
  if (*g_toggle)
    Chip_GPIO_SetValue(LPC_GPIO_PORT, 0, 1 << 8);
  else
    Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1 << 8);

  *g_toggle = !*g_toggle;
  ++*g_i;
#endif
  if (g_tickSinceLastDidSomething < 1000)
    g_tickSinceLastDidSomething++;
}

void DMA_IRQHandler(void) { SPI::getInstance()->onDMAIRQ(); }
}

int main(void) {
  SystemCoreClockUpdate();

  Config_NVIC();
  NVIC_EnableIRQ(M4_IRQn);

#if 0
  Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
  Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1 << 8);
#endif

#if 0
  SPI::init();
  USBHandler::init();
#endif

  for (;;) {
    bool didSomething = false;

#if 0
    if (USBHandler::getInstance()->process())
      didSomething = true;

    if (SPI::getInstance()->callCallbackIfDone())
      didSomething = true;
#endif

    if (didSomething)
      g_tickSinceLastDidSomething = 0;
    else if (g_tickSinceLastDidSomething > 300)
      __WFI();
  }

  return 0;
}
