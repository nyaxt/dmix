#include "spritejson.h"

#include "third_party/picojson.h"
#include <fstream>
#include <iostream>
#include <stdexcept>

namespace {

picojson::value picojsonFromFile(const std::string& filepath) {
  picojson::value v;
  std::ifstream in(filepath);
  in >> v;

  if (v.is<picojson::null>())
    throw std::runtime_error("failed to deserialize jsonfile: " + filepath);

  return v;
}

} // namespace

SpriteMap loadSpriteJson(const std::string& filepath) {
  SpriteMap ret;

  picojson::value v = picojsonFromFile(filepath);
  for (const auto& kv : v.get<picojson::object>()) {
    auto rv = kv.second;
    SpriteRect rect = {static_cast<int>(rv.get("x").get<double>()),
                       static_cast<int>(rv.get("y").get<double>()),
                       static_cast<int>(rv.get("w").get<double>()),
                       static_cast<int>(rv.get("h").get<double>())};
    ret.insert(std::make_pair(kv.first, rect));
  }

  return ret;
}

namespace {

SpriteLayout deserializeSpriteLayout(const picojson::value& varray) {
  SpriteLayout ret;
  const auto& array = varray.get<picojson::array>();
  ret.reserve(array.size());
  for (const auto& v : array) {
    ret.push_back(SpritePlacement{
        v.get("sprite").get<std::string>(),
        static_cast<int>(v.get("dstx").get<double>()),
        static_cast<int>(v.get("dsty").get<double>()),
        static_cast<int>(v.get("dstw").get<double>()),
        static_cast<int>(v.get("dsth").get<double>())});
  }
  return ret;
}

} // namespace

SpriteLayoutMap loadSpriteLayoutMapJson(const std::string& filepath) {
  SpriteLayoutMap ret;

  picojson::value v = picojsonFromFile(filepath);
  for (const auto& kv : v.get<picojson::object>()) {
    auto spriteLayout = deserializeSpriteLayout(kv.second);
    ret.insert(std::make_pair(kv.first, spriteLayout));
  }

  return ret;
}

#ifdef DEFINE_MAIN
int main(int argc, char** argv) {
  auto map = loadSpriteJson("spritetool/splice.json");
  for (const auto& kv : map) {
    std::cout << "key: " << kv.first << " x:" << kv.second.x
              << " y:" << kv.second.y << " w:" << kv.second.w
              << " h:" << kv.second.h << std::endl;
  }

  return 0;
}
#endif
