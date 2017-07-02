/*
 * Based on LPCOpen example code
 *
 * @par
 * Permission to use, copy, modify, and distribute this software and its
 * documentation is hereby granted, under NXP Semiconductors' and its
 * licensor's relevant copyrights in the software, without fee, provided that it
 * is used in conjunction with NXP Semiconductors microcontrollers.  This
 * copyright, permission, and disclaimer notice must appear in all copies of
 * this code.
 */

#include <stdio.h>
#include <string.h>

#include "../../proto.h"
#include "board.h"
#include "spi.h"
#include "uiview.h"
#include "usbhandler.h"
#include "util.h"

static const uint32_t M0APP_BASEADDR = 0x14080000;

void M0App_Boot(uint32_t baseaddr) {
  Chip_RGU_TriggerReset(RGU_M0APP_RST);
  Chip_Clock_Enable(CLK_M4_M0APP);
  Chip_CREG_SetM0AppMemMap(baseaddr);
  Chip_RGU_ClearReset(RGU_M0APP_RST);
}

void M0App_TriggerIPI() {
  __DSB();
  // __SEV();
  LPC_CREG->M4TXEVENT = 0x1;
}

int g_tickSinceLastDidSomething = 0;

void Config_NVIC() {
  NVIC_DisableIRQ(DMA_IRQn);
  NVIC_DisableIRQ(USB0_IRQn);
  // High prio
  NVIC_SetPriority(DMA_IRQn, 0x01);
  // Low prio
  NVIC_SetPriority(USB0_IRQn, 0x02);
}

uint32_t g_ticks = 0;

extern "C" {

void MX_CORE_IRQHandler(void) { Chip_CREG_ClearM0AppEvent(); }

}  // extern "C"

class USBHandlerImpl : public USBHandler {
  void notifyDataRecieved(uint8_t *data, size_t len) override {
    if (len == 0) return;

    USB *usb = USB::getInstance();

    CommandType cmd = static_cast<CommandType>(*data);
    switch (cmd) {
      case CommandType::Echo:
        data += 4;
        len -= 4;
        memcpy(usb->getTxBuf(), data, len);
        usb->enqueueResponse(len);
        break;
      case CommandType::SPI0:
        data += 4;
        len -= 4;
        assert(!SPI::getInstance()->isTransactionActive());
        SPI::getInstance()->sendRecv(0, data, usb->getTxBuf(), len);
        usb->enqueueResponse(len);
        break;
      case CommandType::SPI1:
        data += 4;
        len -= 4;
        assert(!SPI::getInstance()->isTransactionActive());
        SPI::getInstance()->sendRecv(1, data, usb->getTxBuf(), len);
        usb->enqueueResponse(len);
        break;
      case CommandType::SPI_SS:
        assert(len >= 3);
        SPIChip chip = static_cast<SPIChip>(data[1]);
        uint8_t highlow = data[2];

        switch (chip) {
          case SPIChip::DAC:
            Chip_GPIO_SetPinState(LPC_GPIO_PORT, SS_DAC_PORT, SS_DAC_PIN,
                                  highlow);
            break;
          case SPIChip::VOL:
            Chip_GPIO_SetPinState(LPC_GPIO_PORT, SS_VOL_PORT, SS_VOL_PIN,
                                  highlow);
            break;
        }
        break;
    }
  }
};

class CSRDriver {
 public:
  void execute(const CSRCommand&);

 private:
  __attribute__((aligned(4))) uint8_t cmdbuf_[1024];
  __attribute__((aligned(4))) uint8_t spirxbuf_[1024];
};

void CSRDriver::execute(const CSRCommand& cmd) {

}

class Client : public SurfaceClient {
 public:
  Client() {
  }

  void SyncLine(uint8_t* linebuf, int x, int y, int w) final {
    uint8_t* dest = static_cast<uint8_t*>(pixels_) + (y * LCD_WIDTH + x) * 4;

    SPI::getInstance()->sendRecv(1, cmdbuf_, spirx_, len);
  }

};

#include "FreeRTOS.h"
#include "task.h"

USBHandlerImpl g_handler;

int main(void) {
  M0App_Boot(M0APP_BASEADDR);

  SystemCoreClockUpdate();

  SPI::init();
  USB::init(&g_handler);

  xTaskCreate(USB::dispatchvTask, "vUSBTask", 1024, NULL, (tskIDLE_PRIORITY + 2UL), (TaskHandle_t *)nullptr);
  xTaskCreate(USB::dispatchvTask, "vUITask", 1024, NULL, (tskIDLE_PRIORITY + 1UL), (TaskHandle_t *)nullptr);

  vTaskStartScheduler();
}
