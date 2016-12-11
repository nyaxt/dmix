#include "board.h"
#include "util.h"
#include "usbhandler.h"
#include "spi.h"

#include <cr_section_macros.h>

int* g_toggle = (int*)0x2000c000;
int* g_i = (int*)0x2000c004;
int* g_j = (int*)0x2000c008;

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

	if (*g_toggle)
		Chip_GPIO_SetValue(LPC_GPIO_PORT, 0, 1<<8);
	else
		Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	*g_toggle = !*g_toggle;
	++ *g_i;
}

void DMA_IRQHandler(void) {
	SPI::getInstance()->onDMAIRQ();
}

}

int main(void) {
    SystemCoreClockUpdate();

    Config_NVIC();
	NVIC_EnableIRQ(M4_IRQn);

	Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	SPI::init();
	USBHandler::init();

	for(;;) {
		bool didSomething = false;

		if (USBHandler::getInstance()->process())
			didSomething = true;

		if (SPI::getInstance()->callCallbackIfDone())
			didSomething = true;

		if (!didSomething)
			__WFI();
	}

    return 0;
}
