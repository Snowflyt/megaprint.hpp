#include <string>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  int i = 42;
  mp::println("int:", i);

  double d = 3.14;
  mp::println("double:", d);

  bool b = true;
  mp::println("bool:", b);

  char c = 'A';
  mp::println("char:", c);

  // By default, string-like types are directly printed
  // To print them with inspection, use 'mp::inspect'
  const char *c_str = "C-style string";
  mp::println("C-style string:", mp::inspect(c_str, mp::option::colors{true}));

  std::string str = "Hello, Megaprint!";
  mp::println("string:", mp::inspect(str, mp::option::colors{true}));

  std::string_view str_view = "String view example";
  mp::println("string_view:", mp::inspect(str_view, mp::option::colors{true}));

  return 0;
}
