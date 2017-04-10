#ifndef _driver_image_h
#define _driver_image_h

#include <functional>
#include <string>
#include <vector>

using WriteToAddrFunc =
    std::function<void(int addrOffset, const std::vector<uint8_t>& data)>;
void submitImage(const std::string& path, WriteToAddrFunc);

#endif  // _driver_image_h
