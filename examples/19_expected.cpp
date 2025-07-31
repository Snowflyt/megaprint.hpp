#include <version>

#ifdef __cpp_lib_expected
#include <expected>
#include <string>
#endif

#include <megaprint/megaprint.hpp>

auto main() -> int {
#ifdef __cpp_lib_expected
  const std::expected<int, std::string> expected{42};
  mp::println("expected int:", expected);

  const std::expected<int, std::string> unexpected{std::unexpect, "error"};
  mp::println("unexpected string:", unexpected);
#endif

  return 0;
}
