#ifndef surface_h
#define surface_h

#include <stdint.h>

constexpr int LCD_WIDTH = 400;
constexpr int LCD_HEIGHT = 96;

constexpr uint8_t COLOR_BLACK = 0x00;
constexpr uint8_t COLOR_BLUE = 0x30;
constexpr uint8_t COLOR_GREEN = 0x0c;
constexpr uint8_t COLOR_RED = 0x03;
constexpr uint8_t COLOR_WHITE = 0x3f;

class SurfaceClient {
 public:
  virtual void SyncLine(uint8_t* linebuf, int x, int y, int w) = 0;
};

class Surface {
 public:
  Surface();

  void SetPixel(int x, int y, uint8_t color) {
    data_[y * LCD_WIDTH + x] = color;
  }
  void FillRect(int x, int y, int w, int h, uint8_t color);
  void DrawCircle(int x, int y, int r, uint8_t color);
  void DrawChar(int x, int y, char c, uint8_t color);
  void DrawString(int x, int y, const char* s, uint8_t color);
  void DrawChar5(int x, int y, char c, uint8_t color);
  void DrawString5(int x, int y, const char* s, uint8_t color);
  void Sync(SurfaceClient* client);

 private:
  uint8_t data_[LCD_WIDTH * LCD_HEIGHT];
  uint8_t linebuf_[LCD_WIDTH * 4];
};

#endif  // surface_h
