#ifndef proto_h
#define proto_h

#include <sys/types.h>

enum class CommandType : uint8_t {
  Echo,
  SPI0,
  SPI1,
  SPI_SS,
};

enum class SPIChip : uint8_t {
  DAC,
  VOL,
};

#endif // proto_h
