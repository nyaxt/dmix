#include "readhex.h"

#include "util.h"

#include <stdio.h>
#include <string.h>
#include <memory>

std::vector<uint8_t> readIntelHexFile(const std::string& path) {
  std::vector<uint8_t> result;

  std::unique_ptr<FILE, decltype(&fclose)> fp(fopen(path.c_str(), "r"), fclose);
  if (!fp) throw ErrnoError("fopen", errno);

  int expNextAddr = 0;
  while (true) {
    char header[9];
    if (fread(header, 1, sizeof(header), fp.get()) != sizeof(header)) {
      if (feof(fp.get())) break;

      throw ErrnoError("fread", errno);
    }

    if (header[0] != ':') {
      throw std::runtime_error("failed to find start code");
    }

    char byteCountStr[3];
    memcpy(byteCountStr, &header[1], 2);
    byteCountStr[2] = '\0';
    int byteCount = strtol(byteCountStr, nullptr, 16);
    if (g_verbose) printf("byteCount: %d\n", byteCount);

    char addrStr[5];
    memcpy(addrStr, &header[3], 4);
    addrStr[4] = '\0';
    int addr = strtol(addrStr, nullptr, 16);
    if (g_verbose) printf("addr: %04x\n", addr);

    if (header[7] != '0') {
      throw std::runtime_error("record type doesn't start with 0");
    }
    if (header[8] == '1') {
      // EOF record
      break;
    } else if (header[8] != '0') {
      throw std::runtime_error(
          stringPrintF("encountered unsupported record type 0%c!", header[8]));
    }

    if (addr != expNextAddr)
      throw std::runtime_error(stringPrintF(
          "Expected addr record %04x, but read %04x", expNextAddr, addr));

    char byteStr[3];
    byteStr[2] = '\0';
    for (int i = 0; i < byteCount; ++i) {
      if (fread(byteStr, 1, 2, fp.get()) != 2)
        throw ErrnoError("fread byteStr", errno);

      int byte = strtol(byteStr, nullptr, 16);
      result.push_back(static_cast<uint8_t>(byte));
    }
    expNextAddr = addr + byteCount;

    if (fread(byteStr, 1, 2, fp.get()) != 2)
      throw ErrnoError("fread checksum", errno);

    while (true) {
      int c = fgetc(fp.get());
      if (c == EOF)
        break;
      else if (!isspace(c)) {
        ungetc(c, fp.get());
        break;
      }
    }
  }

  return result;
}
