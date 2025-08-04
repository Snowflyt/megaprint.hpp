#include <exception>
#include <stdexcept>
#include <system_error>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  try {
    throw std::out_of_range("Index out of range");
  } catch (const std::exception &e) {
    mp::println("Caught exception:", e);
    mp::println("Caught exception:", std::current_exception());
  }

  const std::error_code ec(42, std::generic_category());
  mp::println("Error code:", ec);
  const std::error_condition ec_cond = ec.default_error_condition();
  mp::println("Error condition:", ec_cond);

  return 0;
}
