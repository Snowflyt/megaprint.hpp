#include <doctest/doctest.h>
#include <megaprint/megaprint.hpp>

TEST_CASE("Test bool") {
  REQUIRE(mp::inspect(true) == "true");
  REQUIRE(mp::inspect(false) == "false");
}
