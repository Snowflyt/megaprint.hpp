#include <complex>
#include <numbers>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  mp::set_options(mp::option::numeric_separator{true});

  int i = 1234567890;
  mp::println("int:", i);

  long long ll = 1234567890123456789LL;
  mp::println("long long:", ll);

  float f = 3.14f;
  mp::println("float:", f);

  double pi = std::numbers::pi;
  mp::println("double:", pi);

  long double e = std::numbers::e_v<long double>;
  mp::println("long double:", e);

  std::complex<double> c{3.14, 4.2};
  mp::println("complex number:", c);

  return 0;
}
