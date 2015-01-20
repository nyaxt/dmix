#include "board.h"

#include <cr_section_macros.h>

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

int main(void) {
    SystemCoreClockUpdate();
    Board_Init();

	NVIC_EnableIRQ(M4_IRQn);

	Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	*g_j = 0;
    while(1) {
    	++*g_j;
    	__WFE();
    }

    return 0;
}
