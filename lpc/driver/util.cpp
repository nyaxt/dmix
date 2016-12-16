#include "util.h"

#include <stdarg.h>
#include <stdio.h>

std::vector<uint8_t> parseHex(const std::string& str) {
  std::vector<uint8_t> parsed;
  uint8_t b;
  bool halfbyte_pending = false;

  for (char c : str) {
    uint8_t hb;
    if ('0' <= c && c <= '9')
      hb = c - '0';
    else if ('a' <= c && c <= 'f')
      hb = c - 'a' + 0xa;
    else if ('A' <= c && c <= 'F')
      hb = c - 'A' + 0xa;
    else
      continue;

    if (!halfbyte_pending) {
      b = hb << 4;
      halfbyte_pending = true;
    } else {
      parsed.push_back(b | hb);
      halfbyte_pending = false;
    }
  }

  return parsed;
}

std::string stringPrintF(const char* fmt, ...) {
  va_list arg;
  char tmp[512]; // FIXME

  va_start(arg, fmt);
  vsprintf(tmp, fmt, arg);
  va_end(arg);

  return std::string(tmp);
}

std::string formatHex(const std::vector<uint8_t>& data) {
  // FIXME: not very efficient

  std::string ret;

  for (uint8_t b : data)
    ret += stringPrintF("%02x ", b);

  return std::move(ret);
}
