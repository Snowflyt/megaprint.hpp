#include <vector>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  mp::set_options(mp::option::break_length{0}, mp::option::indent{4},
                  mp::option::trailing_comma{mp::option::trailing_comma_type::always});

  std::vector<int> vec = {1, 2, 3, 4, 5};
  mp::println(vec);

  mp::println(mp::inspect(vec, mp::option::break_length{10}, mp::option::indent{8},
                          mp::option::max_sequence_length{3}));

  return 0;
}
