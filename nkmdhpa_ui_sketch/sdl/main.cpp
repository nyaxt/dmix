#include <SDL.h>
#include "surface.h"
#include "uiview.h"

SDL_Window* g_sdlwin;
SDL_Surface* g_screen_surface;
SDL_Surface* g_lcd_surface;

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

  g_lcd_surface = SDL_CreateRGBSurface(0, LCD_WIDTH, LCD_HEIGHT, 32, 0x0000ff00,
                                       0x00ff0000, 0xff000000, 0x000000ff);

  return true;
}

Surface nkmd_surface;
UIView view(&nkmd_surface);

void update() {
  view.Update();
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
