#include <string>
#include <tuple>
#include <utility>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  const std::pair<int, std::string> pair{42, "foo"};
  mp::println("pair:", pair);

  const std::tuple<int, std::string, int> tpl{42, "foo", 43};
  mp::println("tuple:", tpl);

  return 0;
}
