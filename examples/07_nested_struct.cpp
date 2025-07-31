#include <cstdint>
#include <string>
#include <unordered_set>
#include <vector>

#include <megaprint/megaprint.hpp>

enum class Gender : std::uint8_t {
  male,
  female,
  other,
};

struct Address {
  std::string city;
  std::string street;
  int zip;
};

struct Person {
  std::string name;
  unsigned birth_year;
  Gender sex;
  std::vector<Address> addrs;
  std::unordered_set<std::string> friends;
  Person *self; // self-reference
};

auto main() -> int {
  Person person = {
      .name = "Edgar Allan Poe",
      .birth_year = 1809,
      .sex = Gender::male,
      .addrs = {{
          .city = "Boston",
          .street = "Carver St",
          .zip = 2116,
      }},
      .friends = {"John", "Jane", "Alice"},
  };
  person.self = &person; // self-reference

  mp::println("person:", person);

  return 0;
}
