#include <SDL.h>
#include <math.h>
#include "../fontgen/fontdata.h"
#include "shinhfont.h"

SDL_Window* g_sdlwin;
SDL_Surface* g_screen_surface;
SDL_Surface* g_lcd_surface;

const int LCD_WIDTH = 400;
const int LCD_HEIGHT = 96;

const uint8_t COLOR_BLACK = 0x00;
const uint8_t COLOR_BLUE = 0x30;
const uint8_t COLOR_GREEN = 0x0c;
const uint8_t COLOR_RED = 0x03;
const uint8_t COLOR_WHITE = 0x3f;

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
      e += 2*yy + 1;
    } else {
      xx--;
      e += 2*(yy-xx) + 1; 
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
      for (int c = 0; c < 3; ++c) {
        linebuf_[i * 4 + c] = table_4to256[a & 0x3];
        a = a >> 2;
      }
      linebuf_[i * 4 + 3] = 0xff;
    }
    client->SyncLine(linebuf_, 0, j, LCD_WIDTH);
  }
}

class SDLClient : public SurfaceClient {
 public:
  SDLClient(void* pixels) : pixels_(pixels) {}

  void SyncLine(uint8_t* linebuf, int x, int y, int w) final {
    // printf("syncline %p x: %d, y: %d, w: %d\n", linebuf, x, y, w);
    uint8_t* dest = static_cast<uint8_t*>(pixels_) + (y * LCD_WIDTH + x) * 4;
    memcpy(dest, linebuf, w * 4);
  }

 private:
  void* pixels_;
};

Surface nkmd_surface;

const int MAG = 1;
const int WIN_WIDTH = LCD_WIDTH * MAG;
const int WIN_HEIGHT = LCD_HEIGHT * MAG;

bool init() {
  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    printf("SDL_Init failure: %s\n", SDL_GetError());
    return false;
  }

  g_sdlwin = SDL_CreateWindow("nkmdhpa_ui_sketch", SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED, WIN_WIDTH, WIN_HEIGHT,
                              SDL_WINDOW_SHOWN);
  if (!g_sdlwin) {
    printf("SDL_CreateWindow failure: %s\n", SDL_GetError());
    return false;
  }

  g_screen_surface = SDL_GetWindowSurface(g_sdlwin);

  g_lcd_surface = SDL_CreateRGBSurface(0, LCD_WIDTH, LCD_HEIGHT, 32, 0x000000ff,
                                       0x0000ff00, 0x00ff0000, 0xff000000);

  return true;
}

const int th = 10;

const int st_h = 12;
const int st_top = 96 - st_h;

const int nch = 4;

const int st_padl = 60;
const int st_padr = 1;
const int st_padm = 3;

const int sa_top = 11;
const int sa_bottom = st_top - 10;

const int nband = 64;
const int sa_bwidth = 5;  // Math.ceil(300 / nband);
const int sa_left = 400 - sa_bwidth * nband - 5;
const int sa_maxh = sa_bottom - sa_top;

const int vu_nred = 4;
const int vu_nyellow = 6;
const int vu_ngreen = 10;
const int vu_nbar = vu_nred + vu_nyellow + vu_ngreen;
const int vu_barh = sa_maxh / vu_nbar;

const int vu_bwidth = 10;
const int vu_bspace = 10;
const int vu_left = 10;

void drawBars(Surface* surface, int n, int x, int w) {
  for (int j = 0; j < n; ++j) {
    int y = sa_bottom - (j + 1) * vu_barh;
    int h = vu_barh - 1;

    uint8_t c;
    if (j < vu_ngreen) {
      c = COLOR_GREEN;
    } else if (j < (vu_ngreen + vu_nyellow)) {
      c = COLOR_RED | COLOR_GREEN;
    } else {
      c = COLOR_RED;
    }
    surface->FillRect(x, y, w, h, c);
  }
}

void update() {
  nkmd_surface.FillRect(0, 0, LCD_WIDTH, LCD_HEIGHT, COLOR_BLACK);
  drawBars(&nkmd_surface, random() % (vu_nbar + 1), 8, 20);
  drawBars(&nkmd_surface, random() % (vu_nbar + 1), 41, 20);
  nkmd_surface.DrawString(3 + 30/2 - 2, sa_bottom, "L", COLOR_WHITE);
  nkmd_surface.DrawString(36 + 30/2 - 2, sa_bottom, "R", COLOR_WHITE);

  if (false) {
    nkmd_surface.DrawString(5, st_top,
                            "src: optical 1, 24b / 192kHz. nkmd: reverb 134",
                            COLOR_WHITE);

    if (false) for (int i = 0; i < nband; ++i) {
      const int n = random() % (vu_nbar + 1);
      const int x = sa_left + sa_bwidth * i;
      const int w = sa_bwidth - 1;

      drawBars(&nkmd_surface, n, x, w);
    }

    nkmd_surface.DrawString(3, 0, "-43dB", COLOR_WHITE);
    nkmd_surface.DrawString(36, 0, "-43dB", COLOR_WHITE);
    nkmd_surface.DrawString(250, sa_bottom, "1k", COLOR_WHITE);
    /*
    c.fillText("4k", 370, sa_labely);
    c.fillText("20k", 385, sa_labely);
    */
  } else {
    const int knob_r = 10;
    const int knob_x = 63;

    const int ch_w = (400 - st_padl - st_padr) / nch;
    for (int i = 0; i < nch; ++ i) {
      const int x = st_padl + ch_w * i;

      nkmd_surface.FillRect(x, st_top, ch_w-st_padm, 1, COLOR_WHITE);

      nkmd_surface.DrawString(x+2, st_top, "ch1 coax", COLOR_WHITE);

      drawBars(&nkmd_surface, random() % (vu_nbar + 1), x + 5, 16);
      drawBars(&nkmd_surface, random() % (vu_nbar + 1), x + 30, 16);
      nkmd_surface.DrawString(x + 5 + 16/2 - 1.5, sa_bottom, "L", COLOR_WHITE);
      nkmd_surface.DrawString(x + 30 + 16/2 - 1.5, sa_bottom, "R", COLOR_WHITE);

      nkmd_surface.DrawString5(x + 48.5, 2, "GAIN", COLOR_WHITE);
      const int knob_y = 20;
      const int t = 3.14 * 3 / 4;
      nkmd_surface.DrawCircle(x + knob_x, knob_y, knob_r, COLOR_WHITE);
      /*
      c.beginPath();
      c.moveTo(x + knob_x, knob_y);
      c.lineTo(x + knob_x + knob_r * Math.cos(t), knob_y + knob_r * -Math.sin(t));
      c.stroke();
      */
      nkmd_surface.DrawString(x + 55, 32, "-80dB", COLOR_WHITE);
      nkmd_surface.DrawString5(x + 48, 43, "PAN", COLOR_WHITE);
      nkmd_surface.DrawCircle(x + knob_x, 59, knob_r, COLOR_WHITE);
      nkmd_surface.DrawString(x + 50, 59+13, "L-80dB", COLOR_WHITE);
    }
  }
  SDL_LockSurface(g_lcd_surface);
  {
    SDLClient sdl_client(g_lcd_surface->pixels);
    nkmd_surface.Sync(&sdl_client);
  }
  SDL_UnlockSurface(g_lcd_surface);

  SDL_Rect srcrect = {0, 0, LCD_WIDTH, LCD_HEIGHT};
  SDL_Rect dstrect = {0, 0, WIN_WIDTH, WIN_HEIGHT};
  SDL_BlitScaled(g_lcd_surface, &srcrect, g_screen_surface, &dstrect);
  SDL_UpdateWindowSurface(g_sdlwin);
}

void shutdown() {
  SDL_FreeSurface(g_lcd_surface);
  SDL_FreeSurface(g_screen_surface);
  SDL_DestroyWindow(g_sdlwin);

  SDL_Quit();
}

int main(int argc, char** argv) {
  if (!init()) return 1;

  for (;;) {
    SDL_Event e;
    if (!SDL_PollEvent(&e)) {
      update();
      SDL_Delay(100);
      continue;
    }

    if (e.type == SDL_QUIT) break;
  }

  shutdown();
  return 0;
}
