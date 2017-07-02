#include "csrcommand.h"

#include <stdlib.h>
#include <string.h>

#include "util.h"

void sendCSRCmd(const CSRCommand &csrcmd, uint8_t *txbuf, uint8_t *rxbuf,
                PacketDriver *driver) {
  int wordSize = csrcmd.wordSize();
  constexpr uint8_t cmdSpecial = 0x0f;
  uint8_t cmdByteBase = (csrcmd.isWrite() ? 0x80 : 0x00) | csrcmd.targetByte();

  auto addrC = csrcmd.addr();
  int replyOffset = csrcmd.replyOffset();
  int rxoffset = 0;
  for (int i = 0; i < csrcmd.len();) {
    int left = csrcmd.len() - i;

    int nFrame = 1;
    int nWord;
    uint8_t encodedChunkLen;
    if (left >= wordSize * 16) {
      nFrame = left / (wordSize * 16);
      nWord = 16;
      encodedChunkLen = 0x3 << 5;
    } else if (left >= wordSize * 4) {
      nWord = 4;
      encodedChunkLen = 0x2 << 5;
    } else {
      assert(left >= wordSize);
      nWord = 1;
      encodedChunkLen = 0x1 << 5;
    }
    int chunkLen = wordSize * nWord;

    uint8_t *ptx = txbuf;
    for (int f = 0; f < nFrame; ++f) {
      switch (csrcmd.target()) {
        case CSRTarget::CSR:
          *ptx++ = cmdByteBase | encodedChunkLen | ((addrC >> 8) & 0xf);
          *ptx++ = (addrC >> 0) & 0xff;
          break;
        case CSRTarget::Progrom:
          *ptx++ = cmdByteBase | encodedChunkLen | ((addrC >> 16) & 0xf);
          *ptx++ = (addrC >> 8) & 0xff;
          *ptx++ = (addrC >> 0) & 0xff;
          break;
        case CSRTarget::Dram0:
          *ptx++ = cmdSpecial;
          *ptx++ = cmdByteBase | encodedChunkLen;
          *ptx++ = (addrC >> 24) & 0xff;
          *ptx++ = (addrC >> 16) & 0xff;
          *ptx++ = (addrC >> 8) & 0xff;
          *ptx++ = (addrC >> 0) & 0xff;
          break;
      }
      if (csrcmd.isWrite()) {
        memcpy(ptx, csrcmd.txbody() + i, chunkLen);
        ptx += chunkLen;
      } else {
        memset(ptx, 0xdd, chunkLen);
        ptx += chunkLen;
      }

      i += chunkLen;
      addrC += nWord;
    }

    // NOP padding
    *ptx++ = 0x00;

    driver->executeTransaction(txbuf, rxbuf, ptx - txbuf);
    if (csrcmd.rxbody() &&
        (!csrcmd.isWrite() || csrcmd.target() == CSRTarget::Dram0)) {
      int frameSize = replyOffset + chunkLen - 1;
      for (int f = 0; f < nFrame; ++f) {
        const uint8_t *start = rxbuf + frameSize * f + replyOffset;
        memcpy(csrcmd.rxbody() + rxoffset, start, chunkLen);
        rxoffset += chunkLen;
      }
    }
  }
}
