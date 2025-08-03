#include <vector>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  mp::set_options(mp::option::break_length{0U}, mp::option::indent{4U},
                  mp::option::trailing_comma{mp::option::trailing_comma_type::always});

  std::vector<int> vec = {1, 2, 3, 4, 5};
  mp::println(vec);

  mp::println(mp::inspect(vec, mp::option::break_length{10U}, mp::option::indent{8U},
                          mp::option::max_sequence_length{3U}));

  return 0;
}
