#include <string>

#include <megaprint/megaprint.hpp>

struct Person {
  std::string name;
  unsigned birth_year;
  Person *self; // self-reference
};

auto main() -> int {
  Person person = {
      .name = "Edgar Allan Poe",
      .birth_year = 1809,
      .self = nullptr, // Will be set later
  };
  person.self = &person; // self-reference

  mp::println("person:", person);

  return 0;
}
