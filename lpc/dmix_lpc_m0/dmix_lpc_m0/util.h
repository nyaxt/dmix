#ifndef util_h
#define util_h

#include <stdint.h>
#include <sys/types.h>

#ifdef __cplusplus

extern "C" {
#endif

void die();
void checkAlign(const void*);

#ifdef DEBUG
inline void assert(bool x) { if (!x) die(); }
#else
inline void assert() { }
#endif

#ifdef __cplusplus
}

inline void* operator new(size_t, void* buf) throw() { return buf; }

class CallbackEntryInterface {
public:
	virtual void dispatch(void* thisp) = 0;
	virtual void cleanup(void* thisp) { }

	constexpr static size_t sizeMax = sizeof(void*); // vtbl
};

template<typename Functor>
class CallbackEntry : public CallbackEntryInterface {
public:
	virtual void dispatch(void* thisp) override {
		static_cast<Functor*>(thisp)->operator()();
	}
	virtual void cleanup(void* thisp) override {
		static_cast<Functor*>(thisp)->~Functor();
	}
};

class Callback {
public:
	Callback() {
		invalidate();
	}

	template<typename Functor>
	const Callback& operator=(const Functor& f) {
		static_assert(sizeof(Functor) <= sizeMaxFunctor, "Functor size exceeds max");
		Functor* functorCopy = new(m_memFunctor) Functor(f);
		assert(functorCopy == functor());

		static_assert(sizeof(CallbackEntry<Functor>) <= CallbackEntryInterface::sizeMax, "CallbackEntry size exceeds max");
		CallbackEntryInterface* pEntry = new(m_memEntry) CallbackEntry<Functor>();
		assert(pEntry == entry());

		return *this;
	}

	void operator()() {
		entry()->dispatch(functor());
	}

	void reset() {
		entry()->cleanup(functor());
		m_memFunctor[0] = 0;
	}

private:
	void invalidate() {
		m_memEntry[0] = 0;
		m_memFunctor[0] = 0;
	}

	CallbackEntryInterface* entry() {
		assert(m_memEntry[0]);
		return reinterpret_cast<CallbackEntryInterface*>(m_memEntry);
	}
	void* functor() {
		assert(m_memFunctor[0]);
		return reinterpret_cast<void*>(m_memFunctor);
	}

	uint32_t m_memEntry[CallbackEntryInterface::sizeMax / sizeof(uint32_t)];

	constexpr static size_t sizeMaxFunctor = sizeof(void*) * 4;
	uint32_t m_memFunctor[sizeMaxFunctor / sizeof(uint32_t)];
} __attribute__((aligned(4)));

#endif

#endif
