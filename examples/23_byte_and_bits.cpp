#include <bitset>
#include <cstddef>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  const auto b1 = std::byte{42U};
  mp::println("byte value 1:", b1);

  const auto b2 = std::byte{226U};
  mp::println("byte value 2:", b2);

  std::vector<bool> dynamic_bitset{true, false, true, false};
  mp::println("dynamic bitset:", dynamic_bitset);

  std::bitset<8> static_bitset{0b10101010};
  mp::println("static bitset:", static_bitset);

  return 0;
}
