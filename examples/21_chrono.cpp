#include <chrono>
#include <version>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  using namespace std::chrono;

  const auto now = system_clock::now();
  mp::println("Current time (system_clock):", now);
  const auto now1 = steady_clock::now();
  mp::println("Current time (steady_clock):", now1);
  const auto now2 = high_resolution_clock::now();
  mp::println("Current time (high_resolution_clock):", now2);

#if __cpp_lib_chrono >= 201907L
  const auto now3 = file_clock::now();
  mp::println("Current time (file_clock):", now3);
  const auto now4 = utc_clock::now();
  mp::println("Current time (utc_clock):", now4);
  const auto now5 = tai_clock::now();
  mp::println("Current time (tai_clock):", now5);
  const auto now6 = gps_clock::now();
  mp::println("Current time (gps_clock):", now6);

  auto tz = current_zone();
  mp::println("Current time zone:", tz->name());
  zoned_time local_now{tz, system_clock::now()};
  mp::println("Current local time (zoned_time):", local_now);
#endif

  const auto duration = now.time_since_epoch();
  mp::println("Duration since epoch (system_clock):", duration);
#if __cpp_lib_chrono >= 201907L
  const auto duration_in_years = floor<years>(duration);
  mp::println("Duration since epoch in years:", duration_in_years);
#endif

#if __cpp_lib_chrono >= 201907L
  year_month_day ymd = floor<days>(now);
  mp::println("Current date (year_month_day):", ymd);

  month_day_last mdl = month_day_last{ymd.month()};
  mp::println("Current month and last day (month_day_last):", mdl);
#endif

  return 0;
}
