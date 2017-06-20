#include <SDL.h>

SDL_Window* g_sdlwin;
SDL_Surface* g_screen_surface;
SDL_Surface* g_lcd_surface;

const int LCD_WIDTH = 400;
const int LCD_HEIGHT = 96;

const uint8_t COLOR_BLACK = 0x00;
const uint8_t COLOR_BLUE = 0x30;
const uint8_t COLOR_GREEN = 0x0c;
const uint8_t COLOR_RED = 0x03;

class SurfaceClient {
 public:
  virtual void SyncLine(uint8_t* linebuf, int x, int y, int w) = 0;
};

class Surface {
 public:
  Surface();

  void FillRect(int x, int y, int w, int h, uint8_t color);
  void Sync(SurfaceClient* client);

 private:
  uint8_t data_[LCD_WIDTH * LCD_HEIGHT];
  uint8_t linebuf_[LCD_WIDTH * 4];
};

Surface::Surface() {
  FillRect(0, 0, LCD_WIDTH, LCD_HEIGHT, COLOR_BLACK);
  FillRect(20, 30, 50, 40, COLOR_GREEN);
  FillRect(0, 0, 10, 10, COLOR_RED);
  FillRect(10, 0, 10, 10, COLOR_GREEN);
  FillRect(20, 0, 10, 10, COLOR_BLUE);
}

void Surface::FillRect(int x, int y, int w, int h, uint8_t color) {
  for (int j = y; j < y + h; ++j) {
    for (int i = x; i < x + w; ++i) {
      data_[j * LCD_WIDTH + i] = color;
    }
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

const int MAG = 3;
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

void update() {
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
      SDL_Delay(10);
      continue;
    }

    if (e.type == SDL_QUIT) break;
  }

  shutdown();
  return 0;
}
