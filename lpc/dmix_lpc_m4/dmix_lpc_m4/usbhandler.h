#ifndef usbhandler_h
#define usbhandler_h

#include "app_usbd_cfg.h"
#include "error.h"
#include "lpc_types.h"
#include "usbd_rom_api.h"
#include <sys/types.h>

class USBHandler {
public:
  virtual bool isBusy() { return false; }
  virtual void notifyDataRecieved(uint8_t *data, size_t len) = 0;
};

class USB {
public:
  static void init(USBHandler *handler);
  static inline USB *getInstance() { return s_usb; }

  bool isConnected();
  bool process();

  void onIRQ();

  uint8_t *getTxBuf() { return m_bufTx; }
  void enqueueResponse(size_t len);

private:
  explicit USB(USBHandler *handler);
  ~USB();

  bool hasUnhandledRxData();
  void processRxData();
  void enqueueNextRx();

  static ErrorCode_t dispatchReset(USBD_HANDLE_T);
  ErrorCode_t onReset(USBD_HANDLE_T);
  static ErrorCode_t dispatchConfigure(USBD_HANDLE_T);
  ErrorCode_t onConfigure();
  static ErrorCode_t dispatchControl(USBD_HANDLE_T, void *data, uint32_t event);
  ErrorCode_t onControl(uint32_t event);
  static ErrorCode_t dispatchBulkIn(USBD_HANDLE_T, void *data, uint32_t event);
  ErrorCode_t onBulkIn(uint32_t event);
  static ErrorCode_t dispatchBulkOut(USBD_HANDLE_T, void *data, uint32_t event);
  ErrorCode_t onBulkOut(uint32_t event);
  static ErrorCode_t dispatchInterruptIn(USBD_HANDLE_T, void *data,
                                         uint32_t event);
  ErrorCode_t onInterruptIn(uint32_t event);

  static USB *s_usb;

  const USBD_API_T *m_api = nullptr;
  USBD_HANDLE_T m_handle = nullptr;

  __attribute__((aligned(4))) uint8_t m_bufRx[4096];
  ssize_t m_sizeReceived = -1;
  __attribute__((aligned(4))) uint8_t m_bufTx[4096];
  ssize_t m_sizeSent = -1;

  bool m_pendingBulkIn = false;

  USBHandler *m_handler;
};

#endif // usbhandler_h
