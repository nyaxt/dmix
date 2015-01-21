#include "board.h"
#include "util.h"

#include <cr_section_macros.h>

int* g_toggle = (int*)0x2000c000;
int* g_i = (int*)0x2000c004;
int* g_j = (int*)0x2000c008;

void die()
{
	for(;;)
		;
}

void Dmix_SSP_Init()
{
	Board_SSP_Init(LPC_SSP1);
	Chip_SSP_Init(LPC_SSP1);
	Chip_SSP_SetFormat(LPC_SSP1, SSP_BITS_8, SSP_FRAMEFORMAT_SPI, SSP_CLOCK_MODE0);
	Chip_SSP_Enable(LPC_SSP1);

	Chip_GPDMA_Init(LPC_GPDMA);

	NVIC_DisableIRQ(DMA_IRQn);
	NVIC_DisableIRQ(USB0_IRQn);
	// High prio
	NVIC_SetPriority(DMA_IRQn, 0x01);
	// Low prio
	NVIC_SetPriority(USB0_IRQn, 0x02);
	NVIC_EnableIRQ(DMA_IRQn);

	Chip_SSP_SetMaster(LPC_SSP1, TRUE);
}

#if 0
{
	isDmaTxfCompleted = isDmaRxfCompleted = 0;
	Chip_SSP_DMA_Enable(LPC_SSP1);
	/* data Tx_Buf --> SSP */
	Chip_GPDMA_Transfer(LPC_GPDMA, dmaChSSPTx,
			(uint32_t) &Tx_Buf[0],
			GPDMA_CONN_SSP1_Tx,
			GPDMA_TRANSFERTYPE_M2P_CONTROLLER_DMA,
			BUFFER_SIZE);
	/* data SSP --> Rx_Buf */
	Chip_GPDMA_Transfer(LPC_GPDMA, dmaChSSPRx,
			GPDMA_CONN_SSP1_Rx,
			(uint32_t) &Rx_Buf[0],
			GPDMA_TRANSFERTYPE_P2M_CONTROLLER_DMA,
			BUFFER_SIZE);

	while (!isDmaTxfCompleted || !isDmaRxfCompleted) {}
	Chip_SSP_DMA_Disable(LPC_SSP1);
}
#endif

#include "error.h"
#include "lpc_types.h"
#include "usbd_rom_api.h"
#include "app_usbd_cfg.h"

#include <string.h>

class USBHandler {
public:
	static void init();
	static inline USBHandler* getInstance() { return s_usbHandler; }

	bool isConnected();
	bool process();

	void onIRQ();

private:
	USBHandler();
	~USBHandler();

    static USBHandler* s_usbHandler;

	static ErrorCode_t dispatchReset(USBD_HANDLE_T);
	ErrorCode_t onReset(USBD_HANDLE_T);
	static ErrorCode_t dispatchControl(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onControl(uint32_t event);
	static ErrorCode_t dispatchBulkIn(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onBulkIn(uint32_t event);
	static ErrorCode_t dispatchBulkOut(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onBulkOut(uint32_t event);
	static ErrorCode_t dispatchInterruptIn(USBD_HANDLE_T, void* data, uint32_t event);
	ErrorCode_t onInterruptIn(uint32_t event);

	const USBD_API_T* m_api = nullptr;
	USBD_HANDLE_T m_handle = nullptr;
	uint8_t m_recvBuf[4096];

	bool m_pendingBulkIn = false;
	bool m_pendingBulkOut = false;
	bool m_pendingInterruptIn = false;
};

inline void USBHandler::onIRQ()
{
	m_api->hw->ISR(m_handle);
}

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

USBHandler* USBHandler::s_usbHandler = nullptr;

ErrorCode_t USBHandler::dispatchReset(USBD_HANDLE_T handle) {
	return getInstance()->onReset(handle);
}

ErrorCode_t USBHandler::onReset(USBD_HANDLE_T handle) {
	m_handle = handle;

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
	if (event == USB_EVT_OUT)
		m_pendingBulkOut = true;

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

static uint8_t mem_usbHandler[sizeof(USBHandler)];
void USBHandler::init()
{
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

bool USBHandler::process() {
	if (!isConnected())
		return false;

#if 0
	if (libusbdev_QueueReadDone() != -1) {
		/* Dummy process read data ......*/
		/* requeue read request */
		libusbdev_QueueReadReq(g_rxBuff, PACKET_BUFFER_SIZE);
	}
	if (libusbdev_QueueSendDone() == 0) {
		/* Queue send request */
		libusbdev_QueueSendReq(g_rxBuff, PACKET_BUFFER_SIZE);
	}
#endif

	return true;
}

extern "C" {

void MX_CORE_IRQHandler(void) {
	Chip_CREG_ClearM4Event();

	if (*g_toggle)
		Chip_GPIO_SetValue(LPC_GPIO_PORT, 0, 1<<8);
	else
		Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	*g_toggle = !*g_toggle;
	++ *g_i;
}

void USB_IRQHandler(void) {
	USBHandler::getInstance()->onIRQ();
}

}

int main(void) {
    SystemCoreClockUpdate();
    Board_Init();

	NVIC_EnableIRQ(M4_IRQn);

	Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 8);
	Chip_GPIO_ClearValue(LPC_GPIO_PORT, 0, 1<<8);

	USBHandler::init();
	Dmix_SSP_Init();

	for(;;) {
		bool didSomething = false;

		if (USBHandler::getInstance()->process())
			didSomething = true;

		if (!didSomething)
			__WFI();
	}

    return 0;
}
