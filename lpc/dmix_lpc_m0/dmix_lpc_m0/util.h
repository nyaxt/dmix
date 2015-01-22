#ifndef util_h
#define util_h

#include <sys/types.h>

#ifdef __cplusplus
inline void* operator new(size_t, void* buf) throw() { return buf; }

extern "C" {
#endif

void die();

#ifdef __cplusplus
}
#endif

#endif
