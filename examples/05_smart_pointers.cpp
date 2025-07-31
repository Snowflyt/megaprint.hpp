#include <memory>
#include <string>

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

  mp::println("unique_ptr:", std::make_unique<Person>(person));

  const auto p1 = std::make_shared<Person>(person);
  const auto p2 = p1; // NOLINT
  const auto p3 = p2; // NOLINT
  mp::println("shared_ptr:", p3);

  mp::println("weak_ptr:", std::weak_ptr<Person>(p1));

  return 0;
}
