cmake_minimum_required(VERSION 3.14)

project(megaprint_tests)

option(MEGAPRINT_ENABLE_COVERAGE "Enable test coverage")

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
  # Force to always compile with W4
  if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
    string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
  endif()
elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-long-long -Wpedantic -Wsign-conversion -Wshadow -Wconversion -Werror -Wextra")
endif()

# Enable ASan to detect memory errors on *nix (except macOS + GCC)
if(CMAKE_BUILD_TYPE MATCHES Debug AND UNIX)
  if(NOT (APPLE AND CMAKE_CXX_COMPILER_ID STREQUAL "GNU"))
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
    set(CMAKE_LINKER_FLAGS "${CMAKE_LINKER_FLAGS} -fsanitize=address")
  endif()
endif()

# Disable secure warnings on Windows
if(WIN32)
  add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
endif()

# Use IWYU if found
find_program(IWYU_PATH NAMES include-what-you-use iwyu)

if(IWYU_PATH)
  message(STATUS "Found Include-What-You-Use (IWYU): ${IWYU_PATH}")
  set(IWYU_OPTIONS
    "-Xiwyu" "--mapping_file=${CMAKE_CURRENT_SOURCE_DIR}/../iwyu.imp"
    "-Xiwyu" "--quoted_includes_first"
    "-Xiwyu" "--cxx17ns"
    "-w"
  )

  foreach(project IN LISTS projects)
    set_property(TARGET ${project} PROPERTY CXX_INCLUDE_WHAT_YOU_USE "${IWYU_PATH};${IWYU_OPTIONS}")
  endforeach()
endif()

if(MEGAPRINT_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-O0 -g -fprofile-instr-generate -fcoverage-mapping)
    add_link_options(-fprofile-instr-generate)
  else() # GCC
    add_compile_options(-O0 -g --coverage)
    add_link_options(--coverage)
  endif()
endif()

enable_testing()
include(CTest)

include_directories("../include" ".")
file(GLOB MEGAPRINT_TEST_SOURCES test_*.cpp)

# Generate a translation unit that implements doctest main
set(DOCTEST_MAIN_CPP "${CMAKE_CURRENT_BINARY_DIR}/doctest_main.cpp")
file(GENERATE
  OUTPUT "${DOCTEST_MAIN_CPP}"
  CONTENT "#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN\n#include <doctest/doctest.h>\n"
)

add_executable(megaprint_tests ${MEGAPRINT_TEST_SOURCES} "${DOCTEST_MAIN_CPP}")
set_target_properties(megaprint_tests PROPERTIES OUTPUT_NAME tests)

# Set ${PROJECT_NAME} as the startup project
set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT megaprint_tests)

include(../cmake/CPM.cmake)

CPMAddPackage("gh:doctest/doctest@2.4.12")

target_link_libraries(megaprint_tests PRIVATE doctest::doctest)
