#ifndef _spritejson_h_
#define _spritejson_h_

#include <map>
#include <string>

struct SpriteRect {
  int x, y, w, h;
};

std::map<std::string, SpriteRect> loadSpriteJson(const std::string& filepath);

#endif // _spritejson_h_
