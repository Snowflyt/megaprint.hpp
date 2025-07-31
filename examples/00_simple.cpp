#include <string>

#include <megaprint/megaprint.hpp>

struct Person {
  std::string name;
  unsigned birth_year;
};

auto main() -> int {
  const Person person = {
      .name = "Edgar Allan Poe",
      .birth_year = 1809,
  };

  mp::println(person);
  mp::println("Hello, megaprint!");
  mp::println("Hello,", person, "!");

  return 0;
}
