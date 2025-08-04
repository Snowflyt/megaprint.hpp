#include <atomic>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  const std::atomic<int> atomic_int{42};
  mp::println("atomic int:", atomic_int);

  int n = 42;
  const std::atomic<int *> atomic_ptr{&n};
  mp::println("atomic pointer:", atomic_ptr);

  return 0;
}
