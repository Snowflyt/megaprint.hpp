#include <array>
#include <span>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  int c_nums[4]{1, 2, 3, 4};
  mp::println("c-array:", c_nums);
  mp::println("c-array span", std::span<int>{c_nums});

  std::array<int, 4> nums{1, 2, 3, 4};
  mp::println("std::array:", nums);
  mp::println("std::array span", std::span<int, 4>{nums});

  return 0;
}
