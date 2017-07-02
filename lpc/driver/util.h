#ifndef _driver_util_h
#define _driver_util_h

#include <unistd.h>
#include <stdexcept>
#include <string>
#include <vector>

class noncopyable {
 public:
  noncopyable() {}
  noncopyable(noncopyable&&) = default;
  noncopyable& operator=(noncopyable&&) = default;

 private:
  noncopyable(const noncopyable&) = delete;
  noncopyable& operator=(const noncopyable&) = delete;
};

class ErrnoError : public std::runtime_error {
 public:
  ErrnoError(const std::string& context, int errnoC);
};

extern bool g_verbose;
std::vector<uint8_t> parseHex(const std::string& str);
std::string stringPrintF(const char* fmt, ...);
std::string formatHex(const std::vector<uint8_t>& data);
void writeMemh(const std::string& path, const std::vector<uint8_t>& data);

inline void assert(bool cond) {
  if (!cond) throw std::runtime_error("assert failed");
}

#endif  // _driver_util_h
