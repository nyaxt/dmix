#ifndef spi_h
#define spi_h

#include "util.h"

#include "FreeRTOS.h"
#include "task.h"
#include "semphr.h"

class SPI {
public:
  static void init();
  static inline SPI *getInstance() { return s_spi; }

  void sendRecv(int ch, const uint8_t *txBuf, uint8_t *rxBuf, size_t len);

  void onDMAIRQ();

  inline bool isTransactionActive() const { return m_isTransactionActive; }

private:
  SPI();

  static SPI *s_spi;

  uint8_t m_dmaTx;
  uint8_t m_dmaRx;

  SemaphoreHandle_t m_mutex = nullptr;
  SemaphoreHandle_t m_notifyCompletion = nullptr;
  bool m_isTransactionActive = false;
  bool m_txComplete = false;
  bool m_rxComplete = false;
};

#endif // spi_h
