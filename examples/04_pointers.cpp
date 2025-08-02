#include <cstddef>
#include <string>
#include <vector>

#include <megaprint/megaprint.hpp>

struct Person {
  std::string name;
  unsigned birth_year;
  std::vector<Person *> friends;
};

auto main() -> int {
  std::nullptr_t null_ptr = nullptr;
  mp::println("null pointer:", null_ptr);

  Person p1 = {.name = "Alice", .birth_year = 1990, .friends = {}};
  Person p2 = {.name = "Bob", .birth_year = 1992, .friends = {}};
  Person p3 = {.name = "Charlie", .birth_year = 1991, .friends = {}};

  p1.friends = {&p2, &p3};

  mp::println("person with friends:", p1);

  return 0;
}
