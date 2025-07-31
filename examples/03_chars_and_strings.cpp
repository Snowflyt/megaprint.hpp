#include <string>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  char c = 'A';
  mp::println("char:", c);

  signed char sc = -5;
  mp::println("signed char:", sc);

  unsigned char uc = 255;
  mp::println("unsigned char:", uc);

  wchar_t wc = L'W';
  mp::println("wide char:", wc);

  char8_t u8c = u8'A';
  mp::println("char8_t:", u8c);

  char16_t u16c = u'A';
  mp::println("char16_t:", u16c);

  char32_t u32c = U'A';
  mp::println("char32_t:", u32c);

  // By default, string-like types are directly printed
  // To print them with inspection, use 'mp::inspect'
  const char fixed_size_str[6] = "Hello";
  mp::println("fixed-size string:", mp::inspect(fixed_size_str, mp::option::colors{true}));

  const char *c_str = "C-style string";
  mp::println("C-style string:", mp::inspect(c_str, mp::option::colors{true}));

  std::string str = "Hello, Megaprint!";
  mp::println("string:", mp::inspect(str, mp::option::colors{true}));

  std::string_view str_view = "String view example";
  mp::println("string_view:", mp::inspect(str_view, mp::option::colors{true}));

  std::pmr::string pmr_str = "PMR string example";
  mp::println("PMR string:", mp::inspect(pmr_str, mp::option::colors{true}));

  return 0;
}
