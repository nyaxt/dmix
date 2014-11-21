#ifndef _spritejson_h_
#define _spritejson_h_

#include <map>
#include <vector>
#include <string>

struct SpriteRect {
  int x, y, w, h;
};
typedef std::map<std::string, SpriteRect> SpriteMap;
SpriteMap loadSpriteJson(const std::string& filepath);

struct SpritePlacement {
  std::string spriteName;
  int dstx, dsty, dstw, dsth;
};
typedef std::vector<SpritePlacement> SpriteLayout;
typedef std::map<std::string, SpriteLayout> SpriteLayoutMap;
SpriteLayoutMap loadSpriteLayoutMapJson(const std::string& filepath);

#endif  // _spritejson_h_
