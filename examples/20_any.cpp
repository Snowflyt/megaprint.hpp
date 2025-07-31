#include <any>
#include <string>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  std::any any_int = 42;
  mp::println("any int:", any_int);

  std::any any_str = std::string{"Hello, World!"};
  mp::println("any string:", any_str);

  return 0;
}
