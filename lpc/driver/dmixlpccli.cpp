#include <gflags/gflags.h>
#include <libusb-1.0/libusb.h>
#include <memory>
#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <vector>

#include "../proto.h"
#include "image.h"
#include "readhex.h"
#include "util.h"

DEFINE_int32(verbose, 0, "increase verbosity");
DEFINE_bool(dryrun, false, "test adaptor hardware");
DEFINE_string(prefix, "", "prefix to send in hex, such as \"de,ad,be,ef\"");
DEFINE_string(hex, "", "data to send in hex, such as \"de,ad,be,ef\"");
DEFINE_string(hexfile, "", "intel HEX file to send.");
DEFINE_string(fbimg, "", "img file to submit to the frame buffer.");
DEFINE_string(
    csrcmd, "",
    "nkmd csr_spi cmd. \"[read|write] [csr|progrom|dram0] [offset]\"");
DEFINE_string(dac, "",
              "wm8741 dac board cmd. Specify target chip \"[dac|vol]\".");
DEFINE_string(memh, "", "output SPI cmd to memh file");
bool g_verbose;

static const int USBI2C_VENDOR_ID = 0xF055;
static const int USBI2C_PRODUCT_ID = 0xD316;
static const size_t MAX_BULK_LEN = 0x5000;

struct libusb_context *g_usbctx;

std::vector<uint8_t> prepareTxData() {
  std::vector<uint8_t> txdata;

  if (FLAGS_hex != "") {
    txdata = parseHex(FLAGS_hex);
  }
  if (txdata.size() > MAX_BULK_LEN) {
    throw std::runtime_error("txdata too large!");
  }

  return txdata;
}

class LibUSBError : public std::runtime_error {
public:
  LibUSBError(const char *context, int err)
      : std::runtime_error(
            stringPrintF("%s failed: %s", context,
                         libusb_strerror(static_cast<libusb_error>(err)))) {}
};

class DeviceHandle {
public:
  virtual ~DeviceHandle() {}

  void sendBulk(const std::vector<uint8_t> &txdata) {
    sendBulk(txdata.data(), txdata.size());
  }

  virtual void sendBulk(const uint8_t *data, size_t len) = 0;
  virtual std::vector<uint8_t> recvBulk(size_t len) = 0;
};

class DummyDeviceHandle : public DeviceHandle {
public:
  ~DummyDeviceHandle() {}

  void sendBulk(const uint8_t *data, size_t len) override {
    printf("Tx bulk: %s\n",
           formatHex(std::vector<uint8_t>(data, data + len)).c_str());
    fflush(stdout);
  }
  std::vector<uint8_t> recvBulk(size_t len) override {
    printf("Rx bulk. len %zu\n", len);

    std::vector<uint8_t> ret;
    for (size_t i = 0; i < len; ++i) {
      ret.push_back(static_cast<uint8_t>(i));
    }
    return ret;
  }
};

class USBDeviceHandle : public DeviceHandle, noncopyable {
public:
  static std::unique_ptr<USBDeviceHandle> from(libusb_device *device) {
    return std::unique_ptr<USBDeviceHandle>(new USBDeviceHandle(device));
  }

  USBDeviceHandle(USBDeviceHandle &&) = default;

  ~USBDeviceHandle() {
    if (devhandle_) {
      if (claimed_)
        libusb_release_interface(devhandle_, 0);

      // FIXME: this crashes.
      // libusb_close(devhandle_);
    }
  }

  libusb_device_handle *get() { return devhandle_; }

  std::string getStringDescriptor(uint8_t index) {
    unsigned char tmp[32];
    int err =
        libusb_get_string_descriptor_ascii(devhandle_, index, tmp, sizeof(tmp));
    if (err < 0)
      throw LibUSBError("libusb_get_string_descriptor_ascii", err);

    return std::string(reinterpret_cast<char *>(tmp));
  }

  const libusb_device_descriptor &getDescriptor() {
    if (!fetched_descriptor_) {
      int err = libusb_get_device_descriptor(device_, &descriptor_);
      if (err < 0)
        throw LibUSBError("libusb_get_device_descriptor", err);
      fetched_descriptor_ = true;
    }

    return descriptor_;
  }

  void claim() {
    int err;
#if 0
    err = libusb_set_configuration(devhandle_, 0);
    if (err != 0) throw LibUSBError("libusb_set_configuration", err);

    err = libusb_set_auto_detach_kernel_driver(devhandle_, 1);
    if (err != 0) throw LibUSBError("libusb_set_auto_detach_kernel_driver", err);
#endif

    err = libusb_claim_interface(devhandle_, 0);
    if (err != 0)
      throw LibUSBError("libusb_claim_interface", err);

    claimed_ = true;
  }

  std::string getProductStringDescriptor() {
    return getStringDescriptor(getDescriptor().iProduct);
  }

  using DeviceHandle::sendBulk;
  void sendBulk(const uint8_t *data, size_t len) override;
  std::vector<uint8_t> recvBulk(size_t len) override;

private:
  USBDeviceHandle(libusb_device *device) : device_(device) {
    int err = libusb_open(device, &devhandle_);
    if (err != 0)
      throw LibUSBError("libusb_open", err);
  }

  libusb_device *device_ = nullptr;
  libusb_device_handle *devhandle_ = nullptr;
  bool claimed_ = false;
  bool fetched_descriptor_ = false;
  libusb_device_descriptor descriptor_;
  int timeout_ = 1000;
};

void USBDeviceHandle::sendBulk(const uint8_t *data, size_t len) {
  if (len == 0)
    throw std::runtime_error("test txdata not specified.");

  if (len > MAX_BULK_LEN)
    throw std::runtime_error("txdata too long for a bulk tx");

  if (FLAGS_verbose) {
    printf("Tx bulk: %s ... ",
           formatHex(std::vector<uint8_t>(data, data + len)).c_str());
    fflush(stdout);
  }

  int txlen = 0;
  int success =
      libusb_bulk_transfer(devhandle_, 0x01, const_cast<uint8_t *>(data),
                           static_cast<uint16_t>(len), &txlen, timeout_);
  if (success < 0)
    throw LibUSBError("libusb_bulk_transfer Tx", success);
  if (txlen < len)
    throw std::runtime_error("sent data shorter than expected");
  if (FLAGS_verbose)
    printf("success!\n");
}

std::vector<uint8_t> USBDeviceHandle::recvBulk(size_t len) {
  std::vector<uint8_t> rxdata(len);

  if (FLAGS_verbose) {
    printf("Rx bulk... ");
    fflush(stdout);
  }
  int rxlen = 0;
  int success =
      libusb_bulk_transfer(devhandle_, 0x81, rxdata.data(),
                           static_cast<uint16_t>(len), &rxlen, timeout_);
  if (success < 0)
    throw LibUSBError("libusb_bulk_transfer Rx", success);
  rxdata.resize(rxlen);
  if (FLAGS_verbose)
    printf("Success! Rx: %s\n", formatHex(rxdata).c_str());

  return std::move(rxdata);
}

class USBDeviceList : noncopyable {
public:
  USBDeviceList() {
    ndevices_ = libusb_get_device_list(g_usbctx, &list_);
    if (ndevices_ < 0)
      throw LibUSBError("libusb_get_device_list", ndevices_);
  }

  ~USBDeviceList() { ::libusb_free_device_list(list_, 1); }

  libusb_device **begin() { return list_; }
  libusb_device **end() { return list_ + ndevices_; }

private:
  libusb_device **list_ = nullptr;
  ssize_t ndevices_;
};

std::unique_ptr<USBDeviceHandle> findDevice() {
  USBDeviceList list;
  for (libusb_device *device : list) {
    std::unique_ptr<USBDeviceHandle> devhandle = USBDeviceHandle::from(device);

    if (devhandle->getDescriptor().idVendor != USBI2C_VENDOR_ID)
      continue;
    if (devhandle->getDescriptor().idProduct != USBI2C_PRODUCT_ID)
      continue;
    if (devhandle->getProductStringDescriptor() != "dmix_lpc")
      continue;

    return devhandle;
  }

  throw std::runtime_error("device not found");
}

std::vector<uint8_t> getTxBodyFromFlags() {
  if (FLAGS_hex != "") {
    if (FLAGS_hexfile != "")
      throw std::runtime_error("Specify either --hex or --hexfile. Not both.");

    return parseHex(FLAGS_hex);
  } else if (FLAGS_hexfile != "") {
    if (FLAGS_hex != "")
      throw std::runtime_error("Specify either --hex or --hexfile. Not both.");

    return readIntelHexFile(FLAGS_hexfile);
  } else
    throw std::runtime_error("Specify either --hex or --hexfile.");
}

void cmdDefault(DeviceHandle *devhandle) {
  std::vector<uint8_t> txdata;
  if (FLAGS_prefix != "") {
    txdata = parseHex(FLAGS_prefix);
  }

  std::vector<uint8_t> body = getTxBodyFromFlags();
  txdata.insert(txdata.end(), body.begin(), body.end());

  printf("tx hex len: %zu data: %s\n", txdata.size(),
         formatHex(txdata).c_str());

  devhandle->sendBulk(txdata);
  auto rxdata = devhandle->recvBulk(4096);

  printf("rx hex len: %zu data: %s\n", rxdata.size(),
         formatHex(rxdata).c_str());
}

enum class CSRTarget { CSR, Progrom, Dram0 };

class CSRCommand {
public:
  CSRCommand() = default;
  CSRCommand(CSRTarget target, int addr, const std::vector<uint8_t> &body)
      : is_write_(true), target_(target), addr_(addr), body_(body),
        len_(body.size()) {}

  static CSRCommand parseFlags();

  const bool is_write() const { return is_write_; }
  CSRTarget target() const { return target_; }
  int addr() const { return addr_; }
  const std::vector<uint8_t> &body() const { return body_; }
  int len() const { return len_; }

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

  size_t replyOffset() const {
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
  std::vector<uint8_t> body_;
  int len_;
};

CSRCommand CSRCommand::parseFlags() {
  const auto &cmdstr = FLAGS_csrcmd;
  CSRCommand ret;

  int i = 0;
  for (; i < cmdstr.size(); ++i)
    if (!isspace(cmdstr[i]))
      break;

#define MATCH_PREFIX(prefix)                                                   \
  if (cmdstr.size() >= i + sizeof(prefix) - 1 &&                               \
      cmdstr.substr(i, sizeof(prefix) - 1) == prefix && ({                     \
        i += sizeof(prefix) - 1;                                               \
        true;                                                                  \
      }))

  ret.is_write_ = false;
  MATCH_PREFIX("read ") {}
  else MATCH_PREFIX("write ") {
    ret.is_write_ = true;
  }
  else {
    throw std::runtime_error(stringPrintF(
        "Failed to find read/write in cmd: \"%s\"", cmdstr.c_str()));
  }

  MATCH_PREFIX("csr ") { ret.target_ = CSRTarget::CSR; }
  else MATCH_PREFIX("progrom ") {
    ret.target_ = CSRTarget::Progrom;
  }
  else MATCH_PREFIX("dram0 ") {
    ret.target_ = CSRTarget::Dram0;
  }
  else {
    throw std::runtime_error(stringPrintF(
        "Failed to find valid target in cmd: \"%s\"", cmdstr.c_str()));
  }

#undef MATCH_PREFIX

  for (; i < cmdstr.size(); ++i)
    if (!isspace(cmdstr[i]))
      break;
  int j = i;
  for (; j < cmdstr.size(); ++j)
    if (!isalnum(cmdstr[j]))
      break;
  std::string addrStr = cmdstr.substr(i, j - i);
  int addr = ::strtol(addrStr.c_str(), nullptr, 16);
  if (addr < 0 || addr >= 0xfffffff)
    throw std::runtime_error(stringPrintF("addr %05x out of range", addr));
  if (ret.target() == CSRTarget::CSR && addr >= 0xfff)
    throw std::runtime_error(
        stringPrintF("addr %05x out of range (csr)", addr));
  if (ret.target() == CSRTarget::Progrom && addr >= 0xfffff)
    throw std::runtime_error(
        stringPrintF("addr %05x out of range (progrom)", addr));
  ret.addr_ = addr;

  if (ret.is_write()) {
    ret.body_ = getTxBodyFromFlags();
    ret.len_ = ret.body().size();
  } else {
    i = j;
    for (; i < cmdstr.size(); ++i)
      if (!isspace(cmdstr[i]))
        break;
    j = i;
    for (; j < cmdstr.size(); ++j)
      if (!isalnum(cmdstr[j]))
        break;
    std::string lenStr = cmdstr.substr(i, j - i);
    ret.len_ = ::strtol(lenStr.c_str(), nullptr, 16);
    if (ret.len() < 0 || ret.len() >= 0xfff)
      throw std::runtime_error(
          stringPrintF("len %03x out of range", ret.len()));
  }

  if (g_verbose)
    printf("parsed cmd: isWrite %d target %d addr %07x len %03x\n",
           ret.is_write_, ret.target_, ret.addr_, ret.len_);

  if (ret.len() % ret.wordSize() != 0)
    throw std::runtime_error("body len not multiple of wordSize");

  return ret;
}

void cmdCSRCmd(DeviceHandle *devhandle, const CSRCommand &csrcmd) {
  int wordSize = csrcmd.wordSize();
  constexpr uint8_t cmdSpecial = 0x0f;
  uint8_t cmdByteBase = (csrcmd.is_write() ? 0x80 : 0x00) | csrcmd.targetByte();

  std::vector<uint8_t> rxdata;
  auto addrC = csrcmd.addr();
  size_t replyOffset = csrcmd.replyOffset();
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
      if (left < wordSize)
        throw std::runtime_error("body not multiple of wordSize");
      nWord = 1;
      encodedChunkLen = 0x1 << 5;
    }
    int chunkLen = wordSize * nWord;

    std::vector<uint8_t> txpacket;
    txpacket.push_back(static_cast<uint8_t>(CommandType::SPI1));
    txpacket.push_back(0xc1);
    txpacket.push_back(0xc2);
    txpacket.push_back(0xc3);
    for (int f = 0; f < nFrame; ++f) {
      switch (csrcmd.target()) {
      case CSRTarget::CSR:
        txpacket.push_back(cmdByteBase | encodedChunkLen |
                           ((addrC >> 8) & 0xf));
        txpacket.push_back((addrC >> 0) & 0xff);
        break;
      case CSRTarget::Progrom:
        txpacket.push_back(cmdByteBase | encodedChunkLen |
                           ((addrC >> 16) & 0xf));
        txpacket.push_back((addrC >> 8) & 0xff);
        txpacket.push_back((addrC >> 0) & 0xff);
        break;
      case CSRTarget::Dram0:
        txpacket.push_back(cmdSpecial);
        txpacket.push_back(cmdByteBase | encodedChunkLen);
        txpacket.push_back((addrC >> 24) & 0xff);
        txpacket.push_back((addrC >> 16) & 0xff);
        txpacket.push_back((addrC >> 8) & 0xff);
        txpacket.push_back((addrC >> 0) & 0xff);
        break;
      }
      if (csrcmd.is_write()) {
        for (int j = 0; j < chunkLen; ++j) {
          txpacket.push_back(csrcmd.body()[i + j]);
        }
      } else {
        for (int j = 0; j < chunkLen; ++j) {
          txpacket.push_back(0xdd);
        }
      }

      i += chunkLen;
      addrC += nWord;
    }

    // NOP padding
    txpacket.push_back(0x00);
    devhandle->sendBulk(txpacket);
    if (FLAGS_memh != "")
      throw std::runtime_error("FIXME"); // writeMemh(FLAGS_memh, txpacket);
    std::vector<uint8_t> rxpacket = devhandle->recvBulk(txpacket.size() - 4);
    if (FLAGS_verbose)
      printf("Success! Rx: %s\n", formatHex(rxpacket).c_str());
    if (!csrcmd.is_write() || csrcmd.target() == CSRTarget::Dram0) {
      size_t frameSize = replyOffset + chunkLen - 1;
      for (int f = 0; f < nFrame; ++f) {
        const uint8_t *start = &rxpacket[frameSize * f + replyOffset];
        rxdata.insert(rxdata.end(), start, start + chunkLen);
      }
    }
  }

  // printf("result: %s\n", formatHex(rxdata).c_str());
}

SPIChip parseDACTarget(const std::string &s) {
  if (s == "dac") {
    return SPIChip::DAC;
  } else if (s == "vol") {
    return SPIChip::VOL;
  } else
    throw std::runtime_error(
        stringPrintF("\"%s\" is not a valid dac target", s.c_str()));
}

void setSS(DeviceHandle *devhandle, SPIChip chip, bool highlow) {
  std::vector<uint8_t> txpacket;
  txpacket.push_back(static_cast<uint8_t>(CommandType::SPI_SS));
  txpacket.push_back(static_cast<uint8_t>(chip));
  txpacket.push_back(highlow ? 0xff : 0x00);
  devhandle->sendBulk(txpacket);
}

void cmdDACCmd(DeviceHandle *devhandle, SPIChip chip) {
  setSS(devhandle, chip, false);

  std::vector<uint8_t> txdata;
  txdata.push_back(static_cast<uint8_t>(CommandType::SPI0));
  txdata.push_back(0xc1);
  txdata.push_back(0xc2);
  txdata.push_back(0xc3);
  std::vector<uint8_t> body = getTxBodyFromFlags();
  txdata.insert(txdata.end(), body.begin(), body.end());
  devhandle->sendBulk(txdata);

  std::vector<uint8_t> rxpacket = devhandle->recvBulk(body.size());
  if (FLAGS_verbose)
    printf("Success! Rx: %s\n", formatHex(rxpacket).c_str());

  setSS(devhandle, chip, true);
}

int main(int argc, char *argv[]) {
  gflags::SetUsageMessage(std::string("dmixlpc test client.\n Usage: ") +
                          argv[0]);
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  g_verbose = FLAGS_verbose;

  int ret = 0;
  try {
    if (libusb_init(&g_usbctx) != 0)
      throw std::runtime_error("libusb init failed");

    std::unique_ptr<DeviceHandle> devhandle;
    if (FLAGS_dryrun) {
      devhandle.reset(new DummyDeviceHandle());
    } else {
      std::unique_ptr<USBDeviceHandle> usbdevhandle = findDevice();
      usbdevhandle->claim();
      devhandle = std::move(usbdevhandle);
    }

    if (FLAGS_fbimg != "")
      submitImage(FLAGS_fbimg, [&devhandle](int offset,
                                            const std::vector<uint8_t> &data) {
        cmdCSRCmd(devhandle.get(), CSRCommand(CSRTarget::Dram0, offset, data));
      });
    else if (FLAGS_csrcmd != "")
      cmdCSRCmd(devhandle.get(), CSRCommand::parseFlags());
    else if (FLAGS_dac != "")
      cmdDACCmd(devhandle.get(), parseDACTarget(FLAGS_dac));
    else
      cmdDefault(devhandle.get());
  } catch (std::exception &e) {
    ret = 1;
    fprintf(stderr, "Error: %s\n", e.what());
  }
  libusb_exit(nullptr);
  gflags::ShutDownCommandLineFlags();
  return ret;
}
