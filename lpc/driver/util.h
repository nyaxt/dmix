#ifndef _driver_util_h
#define _driver_util_h

#include <vector>
#include <string>
#include <unistd.h>

class noncopyable {
 public:
  noncopyable() {}
  noncopyable(noncopyable&&) = default;
  noncopyable& operator=(noncopyable&&) = default;

 private:
  noncopyable(const noncopyable&) = delete;
  noncopyable& operator=(const noncopyable&) = delete;
};

std::vector<uint8_t> parseHex(const std::string& str);
std::string stringPrintF(const char* fmt, ...);
std::string formatHex(const std::vector<uint8_t>& data);

#endif // _driver_util_h
