#include <string>
#include <variant>

#include <megaprint/megaprint.hpp>

struct Person {
  std::string name;
  unsigned birth_year;
};

auto main() -> int {
  Person person = {
      .name = "Edgar Allan Poe",
      .birth_year = 1809,
  };

  std::variant<int, const Person *> v1 = 42;
  std::variant<int, const Person *> v2 = &person;

  mp::println("variant 1:", v1);
  mp::println("variant 2:", v2);

  return 0;
}
