#ifndef board_h
#define board_h

#include "chip.h"

#ifdef __cplusplus
extern "C" {
#endif

extern const uint32_t OscRateIn;
extern const uint32_t ExtRateIn;

void Board_SystemInit();

#define SS_DAC_PORT 0
#define SS_DAC_PIN 4
#define SS_VOL_PORT 0
#define SS_VOL_PIN 8

#ifdef __cplusplus
}
#endif

#endif // board_h
