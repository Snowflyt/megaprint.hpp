#include <optional>
#include <string>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  const std::optional<int> opt_int{42};
  mp::println("optional int:", opt_int);

  const std::optional<std::string> opt_str{"foo"};
  mp::println("optional string:", opt_str);

  const std::optional<std::string> nullopt{};
  mp::println("nullopt:", nullopt);

  return 0;
}
