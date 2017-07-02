#ifndef csrcommand_h_
#define csrcommand_h_

#include <stdint.h>

enum class CSRTarget { CSR, Progrom, Dram0 };

class CSRCommand {
 public:
  CSRCommand() = default;
  CSRCommand(CSRTarget target, int addr, uint8_t *txbody, uint8_t *rxbody,
             int len)
      : is_write_(true),
        target_(target),
        addr_(addr),
        txbody_(txbody),
        rxbody_(rxbody),
        len_(len) {}

  bool isWrite() const { return is_write_; }
  void setIsWrite(bool b) { is_write_ = b; }
  CSRTarget target() const { return target_; }
  void setTarget(CSRTarget t) { target_ = t; }
  int addr() const { return addr_; }
  void setAddr(int addr) { addr_ = addr; }
  const uint8_t *txbody() const { return txbody_; }
  void setTxbody(const uint8_t *b) { txbody_ = b; }
  uint8_t *rxbody() const { return rxbody_; }
  void setRxbody(uint8_t *b) { rxbody_ = b; }
  int len() const { return len_; }
  void setLen(int len) { len_ = len; }

  uint8_t targetByte() const {
    switch (target()) {
      case CSRTarget::CSR:
        return 0x00;
      case CSRTarget::Progrom:
        return 0x10;
      case CSRTarget::Dram0:
        return 0x00;
    }
  }

  int wordSize() const {
    switch (target()) {
      case CSRTarget::CSR:
        return 1;
      case CSRTarget::Progrom:
        return 4;
      case CSRTarget::Dram0:
        return 4;
    }
  }

  int replyOffset() const {
    switch (target()) {
      case CSRTarget::CSR:
        return 3;
      case CSRTarget::Progrom:
        return 4;
      case CSRTarget::Dram0:
        return 7;
    }
  }

 private:
  bool is_write_ = false;
  CSRTarget target_;
  int addr_;
  const uint8_t *txbody_ = nullptr;
  uint8_t *rxbody_ = nullptr;
  int len_;
};

class PacketDriver {
 public:
  virtual void executeTransaction(const uint8_t *txbuf, uint8_t *rxbuf,
                                  int len) = 0;
};

void sendCSRCmd(const CSRCommand &csrcmd, uint8_t *txbuf, uint8_t *rxbuf,
                PacketDriver *driver);

#endif  // csrcommand_h_
