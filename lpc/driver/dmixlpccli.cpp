#include <gflags/gflags.h>
#include <libusb-1.0/libusb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdexcept>
#include <vector>

#include "readhex.h"
#include "util.h"

DEFINE_int32(verbose, 0, "increase verbosity");
DEFINE_bool(test, false, "test adaptor hardware");
DEFINE_string(hex, "", "data to send in hex. e.g. \"de,ad,be,ef\"");
DEFINE_string(hexfile, "", "intel HEX file to send");
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
  USBDeviceHandle(libusb_device* device) : device_(device) {
    int err = libusb_open(device, &devhandle_);
    if (err != 0) throw LibUSBError("libusb_open", err);
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
  libusb_device* device_ = nullptr;
  libusb_device_handle* devhandle_ = nullptr;
  bool claimed_ = false;
  bool fetched_descriptor_ = false;
  libusb_device_descriptor descriptor_;
  int timeout_ = 100;
};

void USBDeviceHandle::sendBulk(const uint8_t* data, size_t len) {
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

USBDeviceHandle findDevice() {
  USBDeviceList list;
  for (libusb_device* device : list) {
    USBDeviceHandle devhandle(device);

    if (devhandle.getDescriptor().idVendor != USBI2C_VENDOR_ID) continue;
    if (devhandle.getDescriptor().idProduct != USBI2C_PRODUCT_ID) continue;
    if (devhandle.getProductStringDescriptor() != "dmix_lpc") continue;

    return std::move(devhandle);
  }

  throw std::runtime_error("device not found");
}

std::vector<uint8_t> doTest(USBDeviceHandle* devhandle,
                            std::vector<uint8_t> txdata) {
  if (FLAGS_verbose)
    printf("Sending test data: %s\n", formatHex(txdata).c_str());

  if (txdata.size() == 0)
    throw std::runtime_error("test txdata not specified.");

  if (txdata.size() > MAX_BULK_LEN)
    throw std::runtime_error("test txdata too long.");

  devhandle->sendBulk(txdata);
  sleep(1);
  std::vector<uint8_t> rxdata = devhandle->recvBulk(4096);
  return std::move(rxdata);
}

int main(int argc, char* argv[]) {
  google::SetUsageMessage(std::string("dmixlpc test client.\n Usage: ") +
                          argv[0]);
  google::ParseCommandLineFlags(&argc, &argv, true);
  g_verbose = FLAGS_verbose;

  if (FLAGS_hexfile != "") {
    std::vector<uint8_t> hexdata = readIntelHexFile(FLAGS_hexfile);
    printf("hex len: %zu data: %s\n", hexdata.size(), formatHex(hexdata).c_str());
    return 0;
  }

  try {
    if (libusb_init(&g_usbctx) != 0)
      throw std::runtime_error("libusb init failed");

    printf("ihoge!\n");
    USBDeviceHandle devhandle = findDevice();
    printf("try claim!\n");
    devhandle.claim();
    printf("claimed!\n");

    auto txdata = parseHex(FLAGS_hex);
    auto rxdata = doTest(&devhandle, txdata);
    printf("len: %zu\n", rxdata.size());
    size_t count = 0;
    for (uint8_t byte : rxdata) {
      printf("%02x ", byte);
      if (count++ % 4 == 3) printf("\n");
    }
    printf("\n");
  } catch (std::exception& e) {
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
  }

  libusb_exit(nullptr);
  return 0;
}
