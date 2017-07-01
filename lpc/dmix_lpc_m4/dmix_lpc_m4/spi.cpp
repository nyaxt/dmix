#include "spi.h"

#include "board.h"
#include "util.h"

SPI *SPI::s_spi = nullptr;

static __attribute__((aligned(32))) uint8_t mem_spi[sizeof(SPI)];
void SPI::init() {
  s_spi = reinterpret_cast<SPI *>(mem_spi);
  s_spi = new (reinterpret_cast<void *>(mem_spi)) SPI;
}

SPI::SPI() {
  Chip_SSP_Init(LPC_SSP0);
  Chip_SSP_SetBitRate(LPC_SSP0, 100000);
  Chip_SSP_SetFormat(LPC_SSP0, SSP_BITS_8, SSP_FRAMEFORMAT_SPI,
                     SSP_CLOCK_CPHA0_CPOL0);
  Chip_SSP_Enable(LPC_SSP0);
  Chip_SSP_Init(LPC_SSP1);
  Chip_SSP_SetBitRate(LPC_SSP1, 10000000);
  Chip_SSP_SetFormat(LPC_SSP1, SSP_BITS_8, SSP_FRAMEFORMAT_SPI,
                     SSP_CLOCK_CPHA0_CPOL0);
  Chip_SSP_Enable(LPC_SSP1);

  Chip_GPDMA_Init(LPC_GPDMA);

  NVIC_EnableIRQ(DMA_IRQn);

  Chip_SSP_SetMaster(LPC_SSP0, TRUE);
  Chip_SSP_SetMaster(LPC_SSP1, TRUE);
}

void SPI::doSendRecvImpl(int ch, const uint8_t *txBuf, uint8_t *rxBuf,
                         size_t len) {
  if (m_isTransactionActive) die();

  m_isTransactionActive = true;
  m_txComplete = m_rxComplete = false;

  if (ch == 0) {
    m_dmaTx = Chip_GPDMA_GetFreeChannel(LPC_GPDMA, GPDMA_CONN_SSP0_Tx);
    m_dmaRx = Chip_GPDMA_GetFreeChannel(LPC_GPDMA, GPDMA_CONN_SSP0_Rx);

    Chip_SSP_DMA_Disable(LPC_SSP0);
    checkAlign(txBuf);
    Chip_GPDMA_Transfer(LPC_GPDMA, m_dmaTx, reinterpret_cast<uint32_t>(txBuf),
                        GPDMA_CONN_SSP0_Tx,
                        GPDMA_TRANSFERTYPE_M2P_CONTROLLER_DMA, len);
    checkAlign(rxBuf);
    Chip_GPDMA_Transfer(LPC_GPDMA, m_dmaRx, GPDMA_CONN_SSP0_Rx,
                        reinterpret_cast<uint32_t>(rxBuf),
                        GPDMA_TRANSFERTYPE_P2M_CONTROLLER_DMA, len);
    Chip_SSP_DMA_Enable(LPC_SSP0);
  } else {
    m_dmaTx = Chip_GPDMA_GetFreeChannel(LPC_GPDMA, GPDMA_CONN_SSP1_Tx);
    m_dmaRx = Chip_GPDMA_GetFreeChannel(LPC_GPDMA, GPDMA_CONN_SSP1_Rx);

    Chip_SSP_DMA_Disable(LPC_SSP1);
    checkAlign(txBuf);
    Chip_GPDMA_Transfer(LPC_GPDMA, m_dmaTx, reinterpret_cast<uint32_t>(txBuf),
                        GPDMA_CONN_SSP1_Tx,
                        GPDMA_TRANSFERTYPE_M2P_CONTROLLER_DMA, len);
    checkAlign(rxBuf);
    Chip_GPDMA_Transfer(LPC_GPDMA, m_dmaRx, GPDMA_CONN_SSP1_Rx,
                        reinterpret_cast<uint32_t>(rxBuf),
                        GPDMA_TRANSFERTYPE_P2M_CONTROLLER_DMA, len);
    Chip_SSP_DMA_Enable(LPC_SSP1);
  }
}

void SPI::onDMAIRQ() {
  if (Chip_GPDMA_Interrupt(LPC_GPDMA, m_dmaTx) == SUCCESS) m_txComplete = true;

  if (Chip_GPDMA_Interrupt(LPC_GPDMA, m_dmaRx) == SUCCESS) m_rxComplete = true;
}

bool SPI::isTransactionDone() { return m_txComplete && m_rxComplete; }

bool SPI::callCallbackIfDone() {
  if (!m_isTransactionActive) return false;
  if (!isTransactionDone()) return false;

  taskENTER_CRITICAL();
  Chip_GPDMA_Stop(LPC_GPDMA, m_dmaTx);
  Chip_GPDMA_Stop(LPC_GPDMA, m_dmaRx);
  Chip_SSP_DMA_Disable(LPC_SSP0);
  Chip_SSP_DMA_Disable(LPC_SSP1);
  m_isTransactionActive = false;
  m_callback();
  if (!m_isTransactionActive) m_callback.reset();
  taskEXIT_CRITICAL();

  return true;
}

void SPI::dispatchvTask(void *) { return getInstance()->vTask(); }

void SPI::vTask() {
  for (;;) {
    vTaskDelay(1);

    while (callCallbackIfDone())
      ;
  }
}
