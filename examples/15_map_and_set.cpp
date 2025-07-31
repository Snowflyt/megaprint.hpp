#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  const std::unordered_map<std::string, int> umap = {
      {"foo", 1},
      {"bar", 2},
      {"baz", 3},
      {"qux", 4},
  };
  mp::println("unordered_map:", umap);

  const std::map<std::string, int> map = {
      {"foo", 1},
      {"bar", 2},
      {"baz", 3},
      {"qux", 4},
  };
  mp::println("map:", map);

  const std::unordered_set<std::string> uset = {
      "foo",
      "bar",
      "baz",
      "qux",
  };
  mp::println("unordered_set:", uset);

  const std::set<std::string> set = {
      "foo",
      "bar",
      "baz",
      "qux",
  };
  mp::println("set:", set);

  return 0;
}
