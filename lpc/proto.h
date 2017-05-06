#ifndef proto_h
#define proto_h

#include <sys/types.h>

enum class CommandType : uint8_t {
  Echo,
  SPI0,
  SPI1,
};

#endif // proto_h
