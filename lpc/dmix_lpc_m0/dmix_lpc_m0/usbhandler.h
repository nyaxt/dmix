#ifndef usbhandler_h
#define usbhandler_h

#include "error.h"
#include "lpc_types.h"
#include "usbd_rom_api.h"
#include "app_usbd_cfg.h"
#include <sys/types.h>

class USBHandler {
public:
	static void init();
	static inline USBHandler* getInstance() { return s_usbHandler; }

	bool isConnected();
	bool process();

	void onIRQ();

	void enqueueResponse(size_t len);

private:
	USBHandler();
	~USBHandler();

	bool hasUnhandledRxData();
	void setHandledRxData();
	void processRxData();

	void enqueueNextRx();

	static ErrorCode_t dispatchReset(USBD_HANDLE_T);
	ErrorCode_t onReset(USBD_HANDLE_T);
	static ErrorCode_t dispatchConfigure(USBD_HANDLE_T);
	ErrorCode_t onConfigure();
	static ErrorCode_t dispatchControl(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onControl(uint32_t event);
	static ErrorCode_t dispatchBulkIn(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onBulkIn(uint32_t event);
	static ErrorCode_t dispatchBulkOut(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onBulkOut(uint32_t event);
	static ErrorCode_t dispatchInterruptIn(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onInterruptIn(uint32_t event);

    static USBHandler* s_usbHandler;

	const USBD_API_T* m_api = nullptr;
	USBD_HANDLE_T m_handle = nullptr;

	__attribute__((aligned(4))) uint8_t m_bufRx[4096];
	ssize_t m_sizeReceived = -1;
	__attribute__((aligned(4))) uint8_t m_bufTx[4096];
	ssize_t m_sizeSent = -1;

	bool m_pendingBulkIn = false;
};

#endif // usbhandler_h
