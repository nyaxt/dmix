#include "util.h"

extern "C" {

void die()
{
	for(;;)
		;
}

void checkAlign(const void* ptr) {
	if ((reinterpret_cast<uint32_t>(ptr) & 0x3) != 0)
		die();
}

}
