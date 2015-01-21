#ifndef util_h
#define util_h

#ifdef __cplusplus

inline void* operator new(size_t, void* buf) throw() { return buf; }

#endif

#endif
