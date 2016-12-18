#include <gflags/gflags.h>
#include <libusb-1.0/libusb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <memory>
#include <stdexcept>
#include <vector>

#include "readhex.h"
#include "util.h"

DEFINE_int32(verbose, 0, "increase verbosity");
DEFINE_bool(test, false, "test adaptor hardware");
DEFINE_string(prefix, "", "prefix to send in hex, such as \"de,ad,be,ef\"");
DEFINE_string(hex, "", "data to send in hex, such as \"de,ad,be,ef\"");
DEFINE_string(hexfile, "", "intel HEX file to send.");
DEFINE_string(csrcmd, "",
              "nkmd csr_spi cmd. \"[read|write] [csr|progrom] [offset]");
bool g_verbose;

static const int USBI2C_VENDOR_ID = 0xF055;
static const int USBI2C_PRODUCT_ID = 0xD316;
static const size_t MAX_BULK_LEN = 0x5000;

struct libusb_context* g_usbctx;

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
  LibUSBError(const char* context, int err)
      : std::runtime_error(
            stringPrintF("%s failed: %s", context,
                         libusb_strerror(static_cast<libusb_error>(err)))) {}
};

class USBDeviceHandle : noncopyable {
 public:
  static std::unique_ptr<USBDeviceHandle> from(libusb_device* device) {
    return std::unique_ptr<USBDeviceHandle>(new USBDeviceHandle(device));
  }

  USBDeviceHandle(USBDeviceHandle&&) = default;

  ~USBDeviceHandle() {
    if (devhandle_) {
      if (claimed_) libusb_release_interface(devhandle_, 0);

      // FIXME: this crashes.
      // libusb_close(devhandle_);
    }
  }

  libusb_device_handle* get() { return devhandle_; }

  std::string getStringDescriptor(uint8_t index) {
    unsigned char tmp[32];
    int err =
        libusb_get_string_descriptor_ascii(devhandle_, index, tmp, sizeof(tmp));
    if (err < 0) throw LibUSBError("libusb_get_string_descriptor_ascii", err);

    return std::string(reinterpret_cast<char*>(tmp));
  }

  const libusb_device_descriptor& getDescriptor() {
    if (!fetched_descriptor_) {
      int err = libusb_get_device_descriptor(device_, &descriptor_);
      if (err < 0) throw LibUSBError("libusb_get_device_descriptor", err);
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
    if (err != 0) throw LibUSBError("libusb_claim_interface", err);

    claimed_ = true;
  }

  std::string getProductStringDescriptor() {
    return getStringDescriptor(getDescriptor().iProduct);
  }

  void sendBulk(const std::vector<uint8_t>& txdata) {
    sendBulk(txdata.data(), txdata.size());
  }
  void sendBulk(const uint8_t* data, size_t len);

  std::vector<uint8_t> recvBulk(size_t len);
  // std::vector<uint8_t> recvFeatureReportInterrupt(size_t len);

 private:
  USBDeviceHandle(libusb_device* device) : device_(device) {
    int err = libusb_open(device, &devhandle_);
    if (err != 0) throw LibUSBError("libusb_open", err);
  }

  libusb_device* device_ = nullptr;
  libusb_device_handle* devhandle_ = nullptr;
  bool claimed_ = false;
  bool fetched_descriptor_ = false;
  libusb_device_descriptor descriptor_;
  int timeout_ = 100;
};

void USBDeviceHandle::sendBulk(const uint8_t* data, size_t len) {
  if (len == 0) throw std::runtime_error("test txdata not specified.");

  if (len > MAX_BULK_LEN)
    throw std::runtime_error("txdata too long for a bulk tx");

  if (FLAGS_verbose) {
    printf("Tx bulk: %s ... ",
           formatHex(std::vector<uint8_t>(data, data + len)).c_str());
    fflush(stdout);
  }

  int txlen = 0;
  int success =
      libusb_bulk_transfer(devhandle_, 0x01, const_cast<uint8_t*>(data),
                           static_cast<uint16_t>(len), &txlen, timeout_);
  if (success < 0) throw LibUSBError("libusb_bulk_transfer Tx", success);
  if (txlen < len) throw std::runtime_error("sent data shorter than expected");
  if (FLAGS_verbose) printf("success!\n");
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
  if (success < 0) throw LibUSBError("libusb_bulk_transfer Rx", success);
  rxdata.resize(rxlen);
  if (FLAGS_verbose) printf("Success! Rx: %s\n", formatHex(rxdata).c_str());

  return std::move(rxdata);
}

class USBDeviceList : noncopyable {
 public:
  USBDeviceList() {
    ndevices_ = libusb_get_device_list(g_usbctx, &list_);
    if (ndevices_ < 0) throw LibUSBError("libusb_get_device_list", ndevices_);
  }

  ~USBDeviceList() { ::libusb_free_device_list(list_, 1); }

  libusb_device** begin() { return list_; }
  libusb_device** end() { return list_ + ndevices_; }

 private:
  libusb_device** list_ = nullptr;
  ssize_t ndevices_;
};

std::unique_ptr<USBDeviceHandle> findDevice() {
  USBDeviceList list;
  for (libusb_device* device : list) {
    std::unique_ptr<USBDeviceHandle> devhandle = USBDeviceHandle::from(device);

    if (devhandle->getDescriptor().idVendor != USBI2C_VENDOR_ID) continue;
    if (devhandle->getDescriptor().idProduct != USBI2C_PRODUCT_ID) continue;
    if (devhandle->getProductStringDescriptor() != "dmix_lpc") continue;

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

void cmdDefault(USBDeviceHandle* devhandle) {
  std::vector<uint8_t> txdata;
  if (FLAGS_prefix != "") {
    txdata = parseHex(FLAGS_prefix);
  }

  std::vector<uint8_t> body = getTxBodyFromFlags();
  txdata.insert(txdata.end(), body.begin(), body.end());

  printf("tx hex len: %zu data: %s\n", txdata.size(),
         formatHex(txdata).c_str());

  devhandle->sendBulk(txdata);
  sleep(1);
  auto rxdata = devhandle->recvBulk(4096);
  ;
  printf("rx hex len: %zu data: %s\n", rxdata.size(),
         formatHex(rxdata).c_str());
}

enum class CSRTarget {
  CSR,
  Progrom,
};

void cmdCSRCmd() {
  std::string cmdstr = FLAGS_csrcmd;
  int i = 0;
  for (; i < cmdstr.size(); ++i)
    if (!isspace(cmdstr[i])) break;

#define MATCH_PREFIX(prefix)                               \
  if (cmdstr.size() >= i + sizeof(prefix) - 1 &&           \
      cmdstr.substr(i, sizeof(prefix) - 1) == prefix && ({ \
        i += sizeof(prefix) - 1;                           \
        true;                                              \
      }))

  bool isWrite = false;
  MATCH_PREFIX("read ") {}
  else MATCH_PREFIX("write ") {
    isWrite = true;
  }
  else {
    throw std::runtime_error(stringPrintF(
        "Failed to find read/write in cmd: \"%s\"", cmdstr.c_str()));
  }

  CSRTarget target;
  MATCH_PREFIX("csr ") { target = CSRTarget::CSR; }
  else MATCH_PREFIX("progrom ") {
    target = CSRTarget::Progrom;
  }
  else {
    throw std::runtime_error(stringPrintF(
        "Failed to find valid target in cmd: \"%s\"", cmdstr.c_str()));
  }

#undef MATCH_PREFIX

  for (; i < cmdstr.size(); ++i)
    if (!isspace(cmdstr[i])) break;
  int j = i;
  for (; j < cmdstr.size(); ++j)
    if (!isalnum(cmdstr[j])) break;
  std::string addrStr = cmdstr.substr(i, j - i);
  int addr = ::strtol(addrStr.c_str(), nullptr, 16);
  if (addr < 0 || addr >= 0xfffff)
    throw std::runtime_error(stringPrintF("addr %05x out of range", addr));
  if (target == CSRTarget::CSR && addr >= 0xfff)
    throw std::runtime_error(
        stringPrintF("addr %05x out of range (csr)", addr));

  int len;
  if (!isWrite) {
    i = j;
    for (; i < cmdstr.size(); ++i)
      if (!isspace(cmdstr[i])) break;
    j = i;
    for (; j < cmdstr.size(); ++j)
      if (!isalnum(cmdstr[j])) break;
    std::string lenStr = cmdstr.substr(i, j - i);
    len = ::strtol(lenStr.c_str(), nullptr, 16);
    if (len < 0 || len >= 0xfff)
      throw std::runtime_error(stringPrintF("len %05x out of range", addr));
  }

  if (g_verbose)
    printf("parsed cmd: isWrite %d target %d addr %05x len %03x\n", isWrite,
           target, addr, len);

  std::vector<uint8_t> txdata;
  uint8_t targetByte = 0;
  switch (target) {
    case CSRTarget::CSR:
      targetByte = 0x0 << 4;
      addr <<= 8;
      break;
    case CSRTarget::Progrom:
      targetByte = 0x1 << 4;
      break;
  }
  uint8_t cmdbyte = (isWrite ? 0x80 : 0x00) | targetByte | ((addr >> 16) & 0xf);
  txdata.push_back(cmdbyte);
  txdata.push_back((addr >> 8) & 0xff);
  if (target == CSRTarget::Progrom) txdata.push_back((addr >> 0) & 0xff);

  if (isWrite) {
    std::vector<uint8_t> body = getTxBodyFromFlags();
    txdata.insert(txdata.end(), body.begin(), body.end());

    printf("tx hex len: %zu data: %s\n", txdata.size(),
           formatHex(txdata).c_str());
  } else {
    size_t fillLen =
        len + 1;  // +1 as nkmd requires one more cycle to start read reply
    for (int i = 0; i < fillLen; ++i) {
      txdata.push_back(0xdd);
    }

    printf("tx hex len: %zu data: %s\n", txdata.size(),
           formatHex(txdata).c_str());
  }
}

int main(int argc, char* argv[]) {
  gflags::SetUsageMessage(std::string("dmixlpc test client.\n Usage: ") +
                          argv[0]);
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  g_verbose = FLAGS_verbose;

  if (libusb_init(&g_usbctx) != 0)
    throw std::runtime_error("libusb init failed");

  std::unique_ptr<USBDeviceHandle> devhandle = findDevice();
  devhandle->claim();

  int ret = 0;
  try {
    if (FLAGS_csrcmd != "")
      cmdCSRCmd();
    else
      cmdDefault(devhandle.get());

    return 0;
  } catch (std::exception& e) {
    fprintf(stderr, "Error: %s\n", e.what());
    ret = 1;
  }
  libusb_exit(nullptr);
  gflags::ShutDownCommandLineFlags();
}
