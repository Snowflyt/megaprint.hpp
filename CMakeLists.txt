cmake_minimum_required(VERSION 3.14)

if(NOT DEFINED PROJECT_NAME)
  set(MEGAPRINT_IS_TOP_LEVEL ON)
  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
else()
  set(MEGAPRINT_IS_TOP_LEVEL OFF)
endif()

project(megaprint
  VERSION 0.1.0
  DESCRIPTION "Human-friendly pretty printer for C++ 20"
  HOMEPAGE_URL "https://github.com/Snowflyt/megaprint.hpp"
  LANGUAGES CXX
)

option(MEGAPRINT_INSTALL "Include an install target" ${MEGAPRINT_IS_TOP_LEVEL})
option(MEGAPRINT_BUILD_TESTS "Build tests" ${MEGAPRINT_IS_TOP_LEVEL})
option(MEGAPRINT_BUILD_EXAMPLES "Build examples")

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

add_library(megaprint INTERFACE)
add_library(megaprint::megaprint ALIAS megaprint)

target_compile_features(megaprint INTERFACE cxx_std_20)
target_include_directories(megaprint INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

if(MEGAPRINT_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

if(MEGAPRINT_BUILD_TESTS)
  add_subdirectory(test)
endif()

# Installation config referred to argparse:
# https://github.com/p-ranav/argparse/blob/d924b84eba1f0f0adf38b20b7b4829f6f65b6570/CMakeLists.txt
if(MEGAPRINT_INSTALL)
  install(TARGETS megaprint EXPORT megaprintConfig)
  install(EXPORT megaprintConfig
          NAMESPACE megaprint::
          DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})
  install(FILES ${CMAKE_CURRENT_LIST_DIR}/include/megaprint/megaprint.hpp
          DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/megaprint)

  set(CONFIG_FILE_NAME_WITHOUT_EXT "${PROJECT_NAME}Config")
  set(CMAKE_CONFIG_FILE_BASENAME "${CMAKE_CURRENT_BINARY_DIR}/${CONFIG_FILE_NAME_WITHOUT_EXT}")
  set(CMAKE_CONFIG_VERSION_FILE_NAME "${CMAKE_CONFIG_FILE_BASENAME}-version.cmake")
  set(CMAKE_CONFIG_FILE_NAME "${CMAKE_CONFIG_FILE_BASENAME}.cmake")

  set(OPTIONAL_ARCH_INDEPENDENT "ARCH_INDEPENDENT")

  write_basic_package_version_file("${CMAKE_CONFIG_VERSION_FILE_NAME}"
    COMPATIBILITY SameMajorVersion
    ${OPTIONAL_ARCH_INDEPENDENT}
  )

  export(EXPORT megaprintConfig
         NAMESPACE megaprint::)

  install(FILES "${CMAKE_CONFIG_VERSION_FILE_NAME}"
          DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

  set(PackagingTemplatesDir "${CMAKE_CURRENT_SOURCE_DIR}/packaging")

  set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
  set(CPACK_PACKAGE_VENDOR "megaprint (C++) developers")
  set(CPACK_PACKAGE_DESCRIPTION "${PROJECT_DESCRIPTION}")
  set(CPACK_DEBIAN_PACKAGE_NAME "${CPACK_PACKAGE_NAME}")
  set(CPACK_RPM_PACKAGE_NAME "${CPACK_PACKAGE_NAME}")
  set(CPACK_PACKAGE_HOMEPAGE_URL "${PROJECT_HOMEPAGE_URL}")
  set(CPACK_PACKAGE_MAINTAINER "Ge Gao (Snowflyt)")
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "${CPACK_PACKAGE_MAINTAINER}")
  set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
  set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

  set(CPACK_DEBIAN_PACKAGE_NAME "lib${PROJECT_NAME}-dev")
  set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6-dev")
  set(CPACK_DEBIAN_PACKAGE_SUGGESTS "cmake, pkg-config, pkg-conf")

  set(CPACK_RPM_PACKAGE_NAME "lib${PROJECT_NAME}-devel")
  set(CPACK_RPM_PACKAGE_SUGGESTS "${CPACK_DEBIAN_PACKAGE_SUGGESTS}")

  set(CPACK_DEB_COMPONENT_INSTALL ON)
  set(CPACK_RPM_COMPONENT_INSTALL ON)
  set(CPACK_NSIS_COMPONENT_INSTALL ON)
  set(CPACK_DEBIAN_COMPRESSION_TYPE "xz")

  set(PKG_CONFIG_FILE_NAME "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc")
  configure_file("${PackagingTemplatesDir}/pkgconfig.pc.in" "${PKG_CONFIG_FILE_NAME}" @ONLY)
  install(FILES "${PKG_CONFIG_FILE_NAME}"
          DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")
endif()

include(CPack)
