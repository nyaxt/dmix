#ifndef spi_h
#define spi_h

#include "util.h"

class SPI {
public:
	static void init();
	static inline SPI* getInstance() { return s_spi; }

	template<typename Functor>
	void doSendRecv(const uint8_t* txBuf, uint8_t* rxBuf, size_t len, const Functor& f)
	{
		doSendRecvImpl(txBuf, rxBuf, len);
		m_callback = f;
	}
	void onDMAIRQ();

	void callCallbackIfDone();

private:
	void doSendRecvImpl(const uint8_t* txBuf, uint8_t* rxBuf, size_t len);

	SPI();

	bool isTransactionDone();
	void reset();

	static SPI* s_spi;

	uint8_t m_dmaTx;
	uint8_t m_dmaRx;

	bool m_isTransactionActive = false;
	bool m_txComplete = false;
	bool m_rxComplete = false;
	Callback m_callback;
};

#endif // spi_h
