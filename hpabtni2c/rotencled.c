#include "stm8s.h"
#include "stm8s_i2c.h"
#include "stm8s_uart1.h"

typedef enum {
  DEVI2C_AWAIT_ADDR,
  DEVI2C_AWAIT_DATA,
} devi2c_state_t;
devi2c_state_t g_devi2c_state;
uint8_t g_devi2c_addr;

#define NPWM 64
uint8_t g_pwm[NPWM];

int8_t g_rotenc;

void init(void) {
  g_devi2c_state = DEVI2C_AWAIT_ADDR;
  g_devi2c_addr = 0;
  g_rotenc = 32;

  GPIO_DeInit(GPIOA);
  GPIO_Init(GPIOA, GPIO_PIN_3, GPIO_MODE_OUT_PP_LOW_FAST);
  GPIO_DeInit(GPIOD);
  GPIO_Init(GPIOD, GPIO_PIN_2, GPIO_MODE_IN_FL_NO_IT);
  GPIO_Init(GPIOD, GPIO_PIN_3, GPIO_MODE_IN_FL_NO_IT);

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

  SPI_DeInit();
  SPI_Init(SPI_FIRSTBIT_MSB, SPI_BAUDRATEPRESCALER_2, SPI_MODE_MASTER,
           SPI_CLOCKPOLARITY_LOW, SPI_CLOCKPHASE_1EDGE,
           SPI_DATADIRECTION_1LINE_TX, SPI_NSS_SOFT, 0);
  SPI_Cmd(ENABLE);

  enableInterrupts();
}

INTERRUPT_HANDLER(I2C_IRQHandler, 19) {
  uint8_t data;
  I2C_Event_TypeDef e = I2C_GetLastEvent();
  I2C->SR2 = 0U;
  switch (e) {
  case I2C_EVENT_SLAVE_TRANSMITTER_ADDRESS_MATCHED:
    break;

  case I2C_EVENT_SLAVE_BYTE_TRANSMITTING:
    I2C_SendData(g_rotenc);
    break;
#if 0
    I2C_SendData(data);
    if (g_devi2c_addr < NPWM)
      data = g_pwm[g_devi2c_addr];
    else
      data = g_rotenc;
    I2C_SendData(data);
    g_devi2c_addr++;
    break;
#endif

  case I2C_EVENT_SLAVE_RECEIVER_ADDRESS_MATCHED:
    g_devi2c_state = DEVI2C_AWAIT_ADDR;
    break;

  case I2C_EVENT_SLAVE_BYTE_RECEIVED:
    data = I2C_ReceiveData();
    switch (g_devi2c_state) {
    case DEVI2C_AWAIT_ADDR:
      g_devi2c_addr = data;
      g_devi2c_state = DEVI2C_AWAIT_DATA;
      break;
    case DEVI2C_AWAIT_DATA:
    default:
      if (g_devi2c_addr < NPWM)
        g_pwm[g_devi2c_addr] = data;
      g_devi2c_addr++;
      break;
    }
    break;

  case I2C_EVENT_SLAVE_STOP_DETECTED:
    // clear STOPF flag
    I2C_AcknowledgeConfig(I2C_ACK_CURR);
    break;

  default:
    break;
  }
}

uint8_t ledMakeData(uint8_t *pwm, uint8_t t) {
  int i;
  uint8_t data = 0;

  for (i = 0; i < 8; ++i) {
    data = data << 1;
    if (t < *pwm++)
      data |= 0x01;
  }

  return data;
}

#define wait_txe()                                                             \
  while ((SPI->SR & SPI_SR_TXE) == 0)                                          \
    ;

void ledSend(uint8_t t) {
  int j;
  uint8_t data;
  uint8_t *ppwm = g_pwm;

  for (j = 0; j < (NPWM / 8); ++j) {
    wait_txe();
    data = ledMakeData(ppwm, t);
    SPI_SendData(data);
    ppwm += 8;
  }
  wait_txe();

  GPIO_WriteHigh(GPIOA, GPIO_PIN_3);
  GPIO_WriteLow(GPIOA, GPIO_PIN_3);
}

void ledClear(void) {
  int j;

  for (j = 0; j < (NPWM / 8); ++j) {
    wait_txe();
    SPI_SendData(0);
  }
  wait_txe();

  GPIO_WriteHigh(GPIOA, GPIO_PIN_3);
  GPIO_WriteLow(GPIOA, GPIO_PIN_3);
}

uint8_t g_trans;
uint8_t g_prev;
uint8_t g_currkeep;

// http://elm-chan.org/docs/tec/te04.html
static const int8_t g_decodeTable[] = {0, 1, -1, 0,  -1, 0,  0, 1,
                                       1, 0, 0,  -1, 0,  -1, 1, 0};

void decodeRotEnc(void) {
  uint8_t curr = (GPIOD->IDR >> 2) & 0x3;
  uint8_t i;

  if (curr != g_trans) {
    g_trans = curr;
    g_currkeep = 0;
    return;
  }
  g_currkeep++;
  if (g_currkeep < 4) {
    return;
  }

  i = (g_prev << 2) | curr;
  g_rotenc += g_decodeTable[i];

  g_trans = g_prev = curr;
}

int main() {
  uint16_t i;
  uint8_t pwmt = 0;

  init();

  for (i = 0; i < NPWM; ++i) {
    g_pwm[i] = 0;
  }
  // g_pwm[20] = 2;

  for (;;) {
    ledSend(pwmt);
    pwmt += 2;

    ledClear();
    decodeRotEnc();
  }
}
