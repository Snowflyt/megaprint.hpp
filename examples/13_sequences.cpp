#include <deque>
#include <forward_list>
#include <list>
#include <span>
#include <vector>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  std::vector<int> vec{1, 2, 3, 4};
  mp::println("vector:", vec);
  mp::println("vector span:", std::span<int>{vec});

  std::list<int> lst{1, 2, 3, 4};
  mp::println("list:", lst);

  std::forward_list<int> fwd_lst{1, 2, 3, 4};
  mp::println("forward_list:", fwd_lst);

  std::deque<int> deq{1, 2, 3, 4};
  mp::println("deque:", deq);

  return 0;
}
