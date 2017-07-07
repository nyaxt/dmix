#include "stm8s.h"
#include "stm8s_i2c.h"
#include "stm8s_uart1.h"

uint8_t g_btnpressed;
int8_t g_rotenc;

void init(void) {
  g_btnpressed = 0;
  g_rotenc = 32;

  GPIO_DeInit(GPIOA);
  /* LED 1-3 */
  GPIO_Init(GPIOA, GPIO_PIN_1, GPIO_MODE_OUT_PP_LOW_SLOW);
  GPIO_Init(GPIOA, GPIO_PIN_2, GPIO_MODE_OUT_PP_LOW_SLOW);
  GPIO_Init(GPIOA, GPIO_PIN_3, GPIO_MODE_OUT_PP_LOW_SLOW);
  GPIO_DeInit(GPIOC);
  /* btn 1-3 */
  GPIO_Init(GPIOC, GPIO_PIN_3, GPIO_MODE_IN_PU_NO_IT);
  GPIO_Init(GPIOC, GPIO_PIN_4, GPIO_MODE_IN_PU_NO_IT);
  GPIO_Init(GPIOC, GPIO_PIN_5, GPIO_MODE_IN_PU_NO_IT);
  GPIO_DeInit(GPIOD);
  /* ROTENC */
  GPIO_Init(GPIOD, GPIO_PIN_2, GPIO_MODE_IN_PU_NO_IT);
  GPIO_Init(GPIOD, GPIO_PIN_3, GPIO_MODE_IN_PU_NO_IT);
  /* btn 4 (rotenc) */
  GPIO_Init(GPIOD, GPIO_PIN_4, GPIO_MODE_IN_PU_NO_IT);

  CLK_DeInit();
  CLK_SYSCLKConfig(CLK_PRESCALER_CPUDIV1);
  CLK_HSIPrescalerConfig(CLK_PRESCALER_HSIDIV1);
  CLK_ClockSwitchConfig(CLK_SWITCHMODE_AUTO, CLK_SOURCE_HSI, DISABLE,
                        CLK_CURRENTCLOCKSTATE_DISABLE);

  UART1_DeInit();
  UART1_Init((uint32_t)9600, UART1_WORDLENGTH_8D, UART1_STOPBITS_1,
             UART1_PARITY_NO, UART1_SYNCMODE_CLOCK_DISABLE,
             UART1_MODE_TX_ENABLE);

  I2C_DeInit();
  I2C_Init((uint32_t)100000, 0x70 << 1, I2C_DUTYCYCLE_2, I2C_ACK_CURR,
           I2C_ADDMODE_7BIT, 16);
  I2C_ITConfig((I2C_IT_TypeDef)(I2C_IT_ERR | I2C_IT_EVT | I2C_IT_BUF), ENABLE);
  I2C_Cmd(ENABLE);

  enableInterrupts();
}

void setLed(uint8_t d);

INTERRUPT_HANDLER(I2C_IRQHandler, 19) {
  uint8_t data;
  I2C_Event_TypeDef e = I2C_GetLastEvent();
  I2C->SR2 = 0U;
  switch (e) {
  case I2C_EVENT_SLAVE_TRANSMITTER_ADDRESS_MATCHED:
    break;

  case I2C_EVENT_SLAVE_BYTE_TRANSMITTING:
    I2C_SendData(g_btnpressed | (g_rotenc & 0x0f));
    g_btnpressed = 0;
    g_rotenc = 0;
    break;

  case I2C_EVENT_SLAVE_RECEIVER_ADDRESS_MATCHED:
    break;

  case I2C_EVENT_SLAVE_BYTE_RECEIVED:
    data = I2C_ReceiveData();
    setLed(data);
    break;

  case I2C_EVENT_SLAVE_STOP_DETECTED:
    // clear STOPF flag
    I2C_AcknowledgeConfig(I2C_ACK_CURR);
    break;

  default:
    break;
  }
}

#define DEBOUNCE 4

uint8_t g_re_trans;
uint8_t g_re_prev;
uint8_t g_re_currkeep;

// http://elm-chan.org/docs/tec/te04.html
static const int8_t g_decodeTable[] = {0, 1, -1, 0,  -1, 0,  0, 1,
                                       1, 0, 0,  -1, 0,  -1, 1, 0};

void decodeRotEnc(void) {
  uint8_t curr = (GPIOD->IDR >> 2) & 0x3;
  uint8_t i;

  if (curr != g_re_trans) {
    g_re_trans = curr;
    g_re_currkeep = 0;
    return;
  }
  g_re_currkeep++;
  if (g_re_currkeep < DEBOUNCE) {
    return;
  }

  i = (g_re_prev << 2) | curr;
  g_rotenc += g_decodeTable[i];

  g_re_trans = g_re_prev = curr;
}

uint8_t g_btn_trans;
uint8_t g_btn_prev;
uint8_t g_btn_currkeep;

void debounceBtnRead(void) {
  uint8_t curr = ((GPIOC->IDR << 1) & 0x70) | ((GPIOD->IDR << 3) & 0x80);

  if (curr != g_btn_trans) {
    g_btn_trans = curr;
    g_btn_currkeep = 0;
    return;
  }
  g_btn_currkeep++;
  if (g_btn_currkeep < DEBOUNCE) {
    return;
  }

  g_btnpressed |= ~g_btn_prev & curr;
  g_btn_prev = curr;
}

void setLed(uint8_t d) {
  GPIOA->ODR = (d << 1) & 0x0e; 
}

int main() {
  init();

  g_btn_prev = 0;

  for (;;) {
    debounceBtnRead();
    decodeRotEnc();
  }
}
