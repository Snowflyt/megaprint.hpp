#include <filesystem>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  std::filesystem::path p1 = "/usr/local/bin";
  mp::println("Path 1:", p1);

  std::filesystem::path p2 = "C:\\Program Files\\MyApp";
  mp::println("Path 2:", p2);

  std::filesystem::path p3 = "relative/path/to/file.txt";
  mp::println("Relative Path:", p3);

  std::filesystem::path p4 = "/home/user/../user/docs/./file.txt";
  mp::println("Normalized Path:", p4);

  return 0;
}
