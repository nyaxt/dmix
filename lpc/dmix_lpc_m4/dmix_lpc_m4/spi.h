#ifndef spi_h
#define spi_h

#include "util.h"

#include "FreeRTOS.h"
#include "task.h"

class SPI {
public:
  static void init();
  static inline SPI *getInstance() { return s_spi; }

  static void dispatchvTask(void*);

  template <typename Functor>
  void doSendRecv(int ch, const uint8_t *txBuf, uint8_t *rxBuf, size_t len,
                  const Functor &f) {
    taskENTER_CRITICAL();
    doSendRecvImpl(ch, txBuf, rxBuf, len);
    m_callback = f;
    taskEXIT_CRITICAL();
  }

  void onDMAIRQ();

  bool callCallbackIfDone();

  inline bool isTransactionActive() const { return m_isTransactionActive; }

private:
  void vTask();
  void doSendRecvImpl(int ch, const uint8_t *txBuf, uint8_t *rxBuf, size_t len);

  SPI();

  bool isTransactionDone();

  static SPI *s_spi;

  uint8_t m_dmaTx;
  uint8_t m_dmaRx;

  bool m_isTransactionActive = false;
  bool m_txComplete = false;
  bool m_rxComplete = false;
  Callback m_callback;
};

#endif // spi_h
