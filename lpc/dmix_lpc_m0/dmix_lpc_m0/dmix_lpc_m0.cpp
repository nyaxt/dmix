#include "board.h"

#include <cr_section_macros.h>

static volatile int y;
void delay() {
	volatile int x;
	for (x = 0; x < 3000000; ++ x)
		y = x;
}

void MX_CORE_IRQHandler(void) {
	Chip_CREG_ClearM4Event();
}

int main(void) {
    // Read clock settings and update SystemCoreClock variable
    SystemCoreClockUpdate();
    // Set up and initialize all required blocks and
    // functions related to the board hardware
    Board_Init();

	Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

    // Force the counter to be placed into memory
    volatile static int i = 0 ;
    // Enter an infinite loop, just incrementing a counter
    while(1) {
    	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);
    	delay();
    	Chip_GPIO_SetValue(LPC_GPIO_PORT, 0, 1<<8);
    	delay();
    	++ i;
    }
    return 0 ;
}
