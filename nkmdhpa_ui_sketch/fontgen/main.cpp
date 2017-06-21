#include <stdio.h>
#include <stdexcept>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

int main() {
  int width;
  int height;
  int nch;
  unsigned char* pixels = stbi_load("./k12x10.gif", &width, &height, &nch, 3);
  if (!pixels) throw std::runtime_error("stbi_load failed");

  printf("uint8_t FONTDATA[] = {\n");
  for (unsigned char c = '!'; c < '~'; ++ c) {
    unsigned char cdata[8];
    memset(cdata, 0, sizeof cdata);
    int mask = 0x80;
    int j = 0;

    int xo = (static_cast<int>(c - '!') + 1) * 6;
    for (int y = 0; y < 10; ++ y) {
      for (int x = 0; x < 6; ++ x) {
        unsigned char b = (pixels[(y * width + xo + x) * 3] == 0) ? mask : 0;
        cdata[j] = cdata[j] | b;
        mask = mask >> 1;
        if (!mask) {
          mask = 0x80;
          j++;
        }
      }
    }
    printf("0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, 0x%02x, // '%c'\n",
        cdata[0], cdata[1], cdata[2], cdata[3],
        cdata[4], cdata[5], cdata[6], cdata[7],
        c);
  }
  printf("};\n");

  return 0; 
}
