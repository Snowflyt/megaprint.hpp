alias build := build-release

build-debug:
  mkdir -p build && cd build && \
  cmake -G Ninja -DMEGAPRINT_BUILD_EXAMPLES=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ .. && \
  cmake --build . --config Debug

build-release:
  mkdir -p build && cd build && \
  cmake -G Ninja -DMEGAPRINT_BUILD_EXAMPLES=ON -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ .. && \
  cmake --build . --config Release
