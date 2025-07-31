#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  const std::unordered_multimap<std::string, int> umultimap = {
      {"foo", 1},
      {"bar", 2},
      {"baz", 3},
      // Duplicate key
      {"foo", 4},
  };
  mp::println("unordered_multimap:", umultimap);

  const std::multimap<std::string, int> multimap = {
      {"foo", 1},
      {"bar", 2},
      {"baz", 3},
      // Duplicate key
      {"foo", 4},
  };
  mp::println("multimap:", multimap);

  const std::unordered_multiset<std::string> umultiset = {
      "foo",
      "bar",
      "baz",
      // Duplicate element
      "foo",
  };
  mp::println("unordered_multiset:", umultiset);

  const std::multiset<std::string> multiset = {
      "foo",
      "bar",
      "baz",
      // Duplicate element
      "foo",
  };
  mp::println("multiset:", multiset);

  return 0;
}
