#include "image.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

void submitImage(const std::string& path, WriteToAddrFunc write_to_addr) {
  int width;
  int height;
  int nch;
  unsigned char* pixels = stbi_load(path.c_str(), &width, &height, &nch, 3);
  if (!pixels) throw std::runtime_error("stbi_load failed");

  constexpr int fb_width = 400;
  constexpr int fb_height = 96;
  std::vector<uint8_t> line_data(fb_width * 4);
  for (int y = 0; y < fb_height; ++y) {
    if (y < height) {
      for (int x = 0; x < fb_width; ++x) {
        if (x < width) {
          line_data[x * 4 + 1] = pixels[(y * width + x) * 3 + 0];
          line_data[x * 4 + 2] = pixels[(y * width + x) * 3 + 1];
          line_data[x * 4 + 3] = pixels[(y * width + x) * 3 + 2];
        }
      }
    }
    int addr = y << 9;
    write_to_addr(addr, line_data);
  }

  stbi_image_free(pixels);
}
