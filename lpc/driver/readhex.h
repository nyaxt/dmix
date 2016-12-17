#ifndef _driver_readhex_h
#define _driver_readhex_h

#include <unistd.h>
#include <string>
#include <vector>

std::vector<uint8_t> readIntelHexFile(const std::string& path);

#endif  // _driver_readhex_h
