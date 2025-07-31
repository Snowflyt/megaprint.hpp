#include <array>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  int c_nums[4]{1, 2, 3, 4};
  mp::println("c-array:", c_nums);

  std::array<int, 4> nums{1, 2, 3, 4};
  mp::println("std::array:", nums);

  return 0;
}
