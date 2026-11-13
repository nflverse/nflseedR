#!/bin/sh
# Build the clinching engine as WebAssembly (see wasm/README.md).
#
# Usage: wasm/build.sh [out_dir]   (default: wasm/dist)
# Needs emcc on the PATH (source emsdk_env.sh first). Compiles every
# src/*.cpp except the R bindings (bindings.cpp, cpp11.cpp) plus
# wasm/src/clinching.cpp, and copies the JS wrapper next to the output.
#
# STACK_SIZE: compute_clinching_status() keeps large arrays on the stack
# (over 128 KB in the fixtures), more than the 64 KB Emscripten default. 8 MB
# matches the native main-thread stack R runs on.
set -eu
cd "$(dirname "$0")/.."
out=${1:-wasm/dist}
mkdir -p "$out"

sources=$(ls src/*.cpp | grep -v -e '^src/bindings\.cpp$' -e '^src/cpp11\.cpp$')

# shellcheck disable=SC2086
emcc -std=c++20 -O3 -fexceptions \
  -I src \
  $sources wasm/src/clinching.cpp \
  -s MODULARIZE=1 \
  -s EXPORT_ES6=1 \
  -s EXPORT_NAME=createClinchingModule \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s STACK_SIZE=8MB \
  -s ENVIRONMENT=web,worker,node \
  -s EXPORTED_FUNCTIONS=_malloc,_free \
  -s EXPORTED_RUNTIME_METHODS=HEAP32,HEAPF64,UTF8ToString \
  -o "$out/clinching.js"

cp wasm/js/nflseedr-clinching.js "$out/"
ls -l "$out"
