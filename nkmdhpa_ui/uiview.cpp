#include "uiview.h"

#include <math.h>
#include <stdlib.h>

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
const int sa_bwidth = 5;
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

void UIView::Update() {
  surface_->FillRect(0, 0, LCD_WIDTH, LCD_HEIGHT, COLOR_BLACK);
  drawBars(surface_, random() % (vu_nbar + 1), 8, 20);
  drawBars(surface_, random() % (vu_nbar + 1), 41, 20);
  surface_->DrawString(3 + 30 / 2 - 2, sa_bottom, "L", COLOR_WHITE);
  surface_->DrawString(36 + 30 / 2 - 2, sa_bottom, "R", COLOR_WHITE);

  if (false) {
    surface_->DrawString(5, st_top,
                         "src: optical 1, 24b / 192kHz. nkmd: reverb 134",
                         COLOR_WHITE);

    if (false)
      for (int i = 0; i < nband; ++i) {
        const int n = random() % (vu_nbar + 1);
        const int x = sa_left + sa_bwidth * i;
        const int w = sa_bwidth - 1;

        drawBars(surface_, n, x, w);
      }

    surface_->DrawString(3, 0, "-43dB", COLOR_WHITE);
    surface_->DrawString(36, 0, "-43dB", COLOR_WHITE);
    surface_->DrawString(250, sa_bottom, "1k", COLOR_WHITE);
    /*
    c.fillText("4k", 370, sa_labely);
    c.fillText("20k", 385, sa_labely);
    */
  } else {
    const int knob_r = 10;
    const int knob_x = 63;

    const int ch_w = (400 - st_padl - st_padr) / nch;
    for (int i = 0; i < nch; ++i) {
      const int x = st_padl + ch_w * i;

      surface_->FillRect(x, st_top, ch_w - st_padm, 1, COLOR_WHITE);

      surface_->DrawString(x + 2, st_top, "ch1 coax", COLOR_WHITE);

      drawBars(surface_, random() % (vu_nbar + 1), x + 5, 16);
      drawBars(surface_, random() % (vu_nbar + 1), x + 30, 16);
      surface_->DrawString(x + 5 + 16 / 2 - 1.5, sa_bottom, "L", COLOR_WHITE);
      surface_->DrawString(x + 30 + 16 / 2 - 1.5, sa_bottom, "R", COLOR_WHITE);

      surface_->DrawString5(x + 48.5, 2, "GAIN", COLOR_WHITE);
      const int knob_y = 20;
      const int t = 3.14 * 3 / 4;
      surface_->DrawCircle(x + knob_x, knob_y, knob_r, COLOR_WHITE);
      /*
      c.beginPath();
      c.moveTo(x + knob_x, knob_y);
      c.lineTo(x + knob_x + knob_r * Math.cos(t), knob_y + knob_r *
      -Math.sin(t));
      c.stroke();
      */
      surface_->DrawString(x + 55, 32, "-80dB", COLOR_WHITE);
      surface_->DrawString5(x + 48, 43, "PAN", COLOR_WHITE);
      surface_->DrawCircle(x + knob_x, 59, knob_r, COLOR_WHITE);
      surface_->DrawString(x + 50, 59 + 13, "L-80dB", COLOR_WHITE);
    }
  }
}
