#ifndef uiview_h
#define uiview_h

#include "surface.h"

void drawBars(Surface* surface, int n, int x, int w);

class UIView {
 public:
  UIView(Surface* surface) : surface_(surface) {}

  void Update();

 private:
  Surface* surface_;
};

#endif  // uiview_h
