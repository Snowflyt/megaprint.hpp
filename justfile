alias build := build-release

build-debug:
  mkdir -p build && cd build && \
  cmake -G Ninja -DMEGAPRINT_BUILD_EXAMPLES=ON -DMEGAPRINT_ENABLE_COVERAGE=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ .. && \
  cmake --build . --config Debug

build-release:
  mkdir -p build && cd build && \
  cmake -G Ninja -DMEGAPRINT_BUILD_EXAMPLES=ON -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ .. && \
  cmake --build . --config Release

test:
  ./build/test/tests

test-cov:
  LLVM_PROFILE_FILE="coverage/tests.profraw" ./build/test/tests* && \
  llvm-profdata merge -sparse coverage/tests.profraw -o coverage/tests.profdata && \
  llvm-cov export ./build/test/tests* -instr-profile=coverage/tests.profdata --sources include/ --format=lcov > coverage/lcov.info && \
  command -v sed >/dev/null 2>&1 && sed -i "s|SF:$(pwd)/|SF:|g" coverage/lcov.info && \
  llvm-cov report ./build/test/tests* -instr-profile=coverage/tests.profdata --sources include/ | bash scripts/prettify-coverage-report.sh
