#include "spritejson.h"

#include "picojson.h"
#include <fstream>
#include <iostream>

std::map<std::string, SpriteRect> loadSpriteJson(const std::string& filepath)
{
  std::map<std::string, SpriteRect> ret;

  picojson::value v;
  std::ifstream in(filepath);
  in >> v;

  for (const auto& kv : v.get<picojson::object>()) {
    auto rv = kv.second;
    SpriteRect rect = {
      static_cast<int>(rv.get("x").get<double>()),
      static_cast<int>(rv.get("y").get<double>()),
      static_cast<int>(rv.get("w").get<double>()),
      static_cast<int>(rv.get("h").get<double>())
    };
    ret.insert(std::make_pair(kv.first, rect));
  }

  return ret;
}

#ifdef DEFINE_MAIN
int main(int argc, char** argv)
{
  auto map = loadSpriteJson("spritetool/splice.json");
  for (const auto& kv : map) {
    std::cout << "key: " << kv.first
      << " x:" << kv.second.x
      << " y:" << kv.second.y
      << " w:" << kv.second.w
      << " h:" << kv.second.h
      << std::endl; 
  }

  return 0;
}
#endif
