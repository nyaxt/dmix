#include "usbhandler.h"

#include "util.h"
#include <string.h>

#include "spi.h" // FIXME

/* Endpoint 0 patch that prevents nested NAK event processing */
static uint32_t g_ep0RxBusy = 0;/* flag indicating whether EP0 OUT/RX buffer is busy. */
static USB_EP_HANDLER_T g_Ep0BaseHdlr;	/* variable to store the pointer to base EP0 handler */

static ErrorCode_t EP0_patch(USBD_HANDLE_T hUsb, void *data, uint32_t event)
{
	switch (event) {
	case USB_EVT_OUT_NAK:
		if (g_ep0RxBusy) {
			/* we already queued the buffer so ignore this NAK event. */
			return LPC_OK;
		}
		else {
			/* Mark EP0_RX buffer as busy and allow base handler to queue the buffer. */
			g_ep0RxBusy = 1;
		}
		break;

	case USB_EVT_SETUP:	/* reset the flag when new setup sequence starts */
	case USB_EVT_OUT:
		/* we received the packet so clear the flag. */
		g_ep0RxBusy = 0;
		break;
	}
	return g_Ep0BaseHdlr(hUsb, data, event);
}

ErrorCode_t USBHandler::dispatchReset(USBD_HANDLE_T handle) {
	return getInstance()->onReset(handle);
}

ErrorCode_t USBHandler::onReset(USBD_HANDLE_T handle) {
	m_handle = handle;

	return LPC_OK;
}

ErrorCode_t USBHandler::dispatchConfigure(USBD_HANDLE_T) {
	return getInstance()->onConfigure();
}

ErrorCode_t USBHandler::onConfigure() {
	enqueueNextRx();
	return LPC_OK;
}

#define DEFINE_DISPATCH_HANDLER(HANDLER_NAME) \
    ErrorCode_t USBHandler::dispatch##HANDLER_NAME(USBD_HANDLE_T, void* data, uint32_t event) { \
		USBHandler* usbHandler = static_cast<USBHandler*>(data); \
		return usbHandler->on##HANDLER_NAME(event); \
    }

DEFINE_DISPATCH_HANDLER(Control);
DEFINE_DISPATCH_HANDLER(BulkIn);
DEFINE_DISPATCH_HANDLER(BulkOut);
DEFINE_DISPATCH_HANDLER(InterruptIn);

#undef DEFINE_DISPATCH_HANDLER

ErrorCode_t USBHandler::onControl(uint32_t event) {
	return ERR_USBD_UNHANDLED;
}

ErrorCode_t USBHandler::onBulkIn(uint32_t event) {
	if (event == USB_EVT_IN)
		m_pendingBulkIn = true;

	return LPC_OK;
}

ErrorCode_t USBHandler::onBulkOut(uint32_t event) {
	if (event != USB_EVT_OUT) return LPC_OK;

    // OUT: Host sends data to device.
	// USB controller has already finished receiving last read request.
	// Record that we have unhandled read result by setting m_sizeReceived.
	m_sizeReceived = m_api->hw->ReadEP(m_handle, LUSB_OUT_EP, m_bufRx);

	return LPC_OK;
}

static uint32_t hoge;
ErrorCode_t USBHandler::onInterruptIn(uint32_t event) {
	if (event == USB_EVT_IN) {
		hoge = 0x12345678;
		m_api->hw->WriteEP(m_handle, LUSB_INT_EP, reinterpret_cast<uint8_t*>(&hoge), sizeof(hoge));
	}

	return LPC_OK;
}

USBHandler* USBHandler::s_usbHandler = nullptr;

static __attribute__((aligned(32))) uint8_t mem_usbHandler[sizeof(USBHandler)];
void USBHandler::init()
{
	s_usbHandler = reinterpret_cast<USBHandler*>(mem_usbHandler);
	s_usbHandler = new(reinterpret_cast<void*>(mem_usbHandler)) USBHandler;
}

USBHandler::USBHandler() {
	/* enable clocks and USB PHY/pads */
	USB_init_pin_clk();

	/* Init USB API structure */
	m_api = reinterpret_cast<const USBD_API_T*>(LPC_ROM_API->usbdApiBase);

	/* initialize call back structures */
	USBD_API_INIT_PARAM_T usb_param;
	::memset((void *) &usb_param, 0, sizeof(USBD_API_INIT_PARAM_T));
	usb_param.usb_reg_base = LPC_USB_BASE;
	usb_param.max_num_ep = USB_MAX_EP_NUM;
	usb_param.mem_base = USB_STACK_MEM_BASE;
	usb_param.mem_size = USB_STACK_MEM_SIZE;
	usb_param.USB_Reset_Event = dispatchReset;
	usb_param.USB_Configure_Event = dispatchConfigure;

	/* Set the USB descriptors */
	USB_CORE_DESCS_T desc;
	desc.device_desc = (uint8_t *) USB_DeviceDescriptor;
	desc.string_desc = (uint8_t *) USB_StringDescriptor;
	desc.high_speed_desc = USB_HsConfigDescriptor;
	desc.full_speed_desc = USB_FsConfigDescriptor;
	desc.device_qualifier = (uint8_t *) USB_DeviceQualifier;

	/* USB Initialization */
	if (m_api->hw->Init(&m_handle, &desc, &usb_param) != LPC_OK) die();

	{
		/*	WORKAROUND for artf45032 ROM driver BUG:
            Due to a race condition there is the chance that a second NAK event will
            occur before the default endpoint0 handler has completed its preparation
            of the DMA engine for the first NAK event. This can cause certain fields
            in the DMA descriptors to be in an invalid state when the USB controller
            reads them, thereby causing a hang.
		 */
		USB_CORE_CTRL_T *pCtrl = (USB_CORE_CTRL_T *) m_handle;
		g_Ep0BaseHdlr = pCtrl->ep_event_hdlr[0];/* retrieve the default EP0_OUT handler */
		pCtrl->ep_event_hdlr[0] = EP0_patch;/* set our patch routine as EP0_OUT handler */
	}

	if (m_api->core->RegisterClassHandler(m_handle, dispatchControl, this) != LPC_OK) die();
	if (m_api->core->RegisterEpHandler(m_handle, 2, dispatchBulkOut, this) != LPC_OK) die();
	if (m_api->core->RegisterEpHandler(m_handle, 3, dispatchBulkIn, this) != LPC_OK) die();
	if (m_api->core->RegisterEpHandler(m_handle, 5, dispatchInterruptIn, this) != LPC_OK) die();

	NVIC_EnableIRQ(LPC_USB_IRQ);
	m_api->hw->Connect(m_handle, 1);
}

bool USBHandler::isConnected() {
	return m_handle && USB_IsConfigured(m_handle);
}

inline bool USBHandler::hasUnhandledRxData() { return m_sizeReceived != -1; }
inline void USBHandler::setHandledRxData() { m_sizeReceived = -1; }

void USBHandler::enqueueNextRx() {
	m_api->hw->ReadReqEP(m_handle, LUSB_OUT_EP, m_bufRx, sizeof(m_bufRx));
}

bool USBHandler::process() {
	if (!isConnected())
		return false;

	if (!hasUnhandledRxData())
		return false;

    processRxData();

    setHandledRxData();
    enqueueNextRx();

	return true;
}

void USBHandler::enqueueResponse(size_t len) {
	m_api->hw->WriteEP(m_handle, LUSB_IN_EP, m_bufTx, len);
}

void USBHandler::processRxData() {
	size_t len = static_cast<size_t>(m_sizeReceived);
#if 1
	if (len < sizeof(uint32_t)*2)
		return;

	uint32_t* a = reinterpret_cast<uint32_t*>(m_bufRx);
	*reinterpret_cast<uint32_t*>(m_bufTx) = a[0] + a[1];
	USBHandler::getInstance()->enqueueResponse(4);
#else
	if (!SPI::getInstance()->isTransactionActive()) {
        SPI::getInstance()->doSendRecv(m_bufRx, m_bufTx, len, [len]() {
    		USBHandler::getInstance()->enqueueResponse(len);
    	});
	}
#endif
}

inline void USBHandler::onIRQ()
{
	m_api->hw->ISR(m_handle);
}

extern "C" {

void USB_IRQHandler(void) {
	USBHandler::getInstance()->onIRQ();
}

}
