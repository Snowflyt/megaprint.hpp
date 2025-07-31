cmake_minimum_required(VERSION 3.14)

project(megaprint_tests)

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
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}
    -Wall -Wno-long-long -Wpedantic -Wsign-conversion
    -Wshadow -Wconversion -Werror -Wextra")
endif()

# Enable ASan to detect memory errors on *nix
if(CMAKE_BUILD_TYPE MATCHES Debug AND UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -fno-omit-frame-pointer")
  set(CMAKE_LINKER_FLAGS "${CMAKE_LINKER_FLAGS} -fsanitize=address")
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

enable_testing()
include(CTest)

include_directories("../include" ".")
file(GLOB MEGAPRINT_TEST_SOURCES test_*.cpp)
add_executable(megaprint_tests ${MEGAPRINT_TEST_SOURCES})
set_target_properties(megaprint_tests PROPERTIES OUTPUT_NAME tests)

# Set ${PROJECT_NAME} as the startup project
set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT megaprint_tests)

include(../cmake/CPM.cmake)

CPMAddPackage(
  URI "gh:doctest/doctest@2.4.12"
  OPTIONS "DOCTEST_WITH_MAIN=ON"
)

target_link_libraries(megaprint_tests PRIVATE doctest::doctest)
target_compile_definitions(megaprint_tests PRIVATE DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN)