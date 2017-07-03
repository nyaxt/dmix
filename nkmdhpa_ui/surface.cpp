#include "surface.h"

#include "fontdata.h"
#include "shinhfont.h"

Surface::Surface() { FillRect(0, 0, LCD_WIDTH, LCD_HEIGHT, COLOR_BLACK); }

void Surface::FillRect(int x, int y, int w, int h, uint8_t color) {
  for (int j = y; j < y + h; ++j) {
    for (int i = x; i < x + w; ++i) {
      data_[j * LCD_WIDTH + i] = color;
    }
  }
}

void Surface::DrawCircle(int x, int y, int r, uint8_t color) {
  int xx = r;
  int yy = 0;
  int e = 0;

  while (xx >= yy) {
    SetPixel(x + xx, y + yy, color);
    SetPixel(x - xx, y + yy, color);
    SetPixel(x + xx, y - yy, color);
    SetPixel(x - xx, y - yy, color);
    SetPixel(x + yy, y + xx, color);
    SetPixel(x - yy, y + xx, color);
    SetPixel(x + yy, y - xx, color);
    SetPixel(x - yy, y - xx, color);
    yy++;
    if (e <= 0) {
      e += 2 * yy + 1;
    } else {
      xx--;
      e += 2 * (yy - xx) + 1;
    }
  }
}

void Surface::DrawChar(int x, int y, char c, uint8_t color) {
  if (c < '!' || '~' < c) return;

  uint8_t* p = &FONTDATA[static_cast<int>(c - '!') * 8];
  uint8_t d = *p;

  int mask = 0x80;
  for (int yy = 0; yy < 10; ++yy) {
    for (int xx = 0; xx < 6; ++xx) {
      if (d & mask) data_[(y + yy) * LCD_WIDTH + x + xx] = color;

      mask = mask >> 1;
      if (!mask) {
        mask = 0x80;
        p++;
        d = *p;
      }
    }
  }
}

void Surface::DrawString(int x, int y, const char* s, uint8_t color) {
  char c;
  while ((c = *s++)) {
    DrawChar(x, y, c, color);
    x += 6;
  }
}

void Surface::DrawChar5(int x, int y, char c, uint8_t color) {
  if (c < '!' || '~' < c) return;
  uint32_t d = SHINHFONT[c - '!'];

  for (int yy = 0; yy < 5; ++yy) {
    for (int xx = 0; xx < 5; ++xx) {
      if (d & 1) data_[(y + yy) * LCD_WIDTH + x + xx] = color;

      d >>= 1;
    }
  }
}

void Surface::DrawString5(int x, int y, const char* s, uint8_t color) {
  char c;
  while ((c = *s++)) {
    DrawChar5(x, y, c, color);
    x += 6;
  }
}

void Surface::Sync(SurfaceClient* client) {
  static const uint8_t table_4to256[4] = {0x00, 0x55, 0xaa, 0xff};

  for (int j = 0; j < LCD_HEIGHT; ++j) {
    for (int i = 0; i < LCD_WIDTH; ++i) {
      int a = data_[j * LCD_WIDTH + i];
      for (int c = 1; c < 4; ++c) {
        linebuf_[i * 4 + c] = table_4to256[a & 0x3];
        a = a >> 2;
      }
      linebuf_[i * 4] = 0xff;
    }
    client->SyncLine(linebuf_, 0, j, LCD_WIDTH);
  }
}
