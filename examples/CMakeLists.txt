cmake_minimum_required(VERSION 3.14)

project(megaprint_examples)

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

include_directories("../include" ".")

file(GLOB_RECURSE examples "*.cpp")
foreach(file IN LISTS examples)
  get_filename_component(name ${file} NAME_WE)
  add_executable(megaprint_example_${name} ${file})
  set_target_properties(megaprint_example_${name} PROPERTIES OUTPUT_NAME ${name})
  set_property(TARGET megaprint_example_${name} PROPERTY CXX_STANDARD 20)
endforeach()
