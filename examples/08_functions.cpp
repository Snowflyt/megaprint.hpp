#include <cmath>
#include <functional>

#include <megaprint/megaprint.hpp>

auto add(int a, int b) -> int { return a + b; }

class Point {
public:
  constexpr Point(int x, int y) : x_(x), y_(y) {}

  [[nodiscard]] auto distance_to(const Point &other) const -> double {
    return std::hypot(other.x_ - x_, other.y_ - y_);
  }

private:
  int x_;
  int y_;
};

auto main() -> int {
  mp::println("function:", add);

  mp::println("member function:", &Point::distance_to);

  auto functor = []() {};
  mp::println("functor:", functor);

  std::function<void()> std_func = []() {};
  mp::println("std::function:", std_func);

  return 0;
}
