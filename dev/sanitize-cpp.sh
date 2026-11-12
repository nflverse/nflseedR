#!/bin/sh
# Run the clinching engine under AddressSanitizer and
# UndefinedBehaviorSanitizer without R: the engine is pure C++, so a small
# driver (dev/sanitize/driver.cpp) calls compute_clinching_status() on the
# states after every week of 2006-2025 and on seasons with random results,
# with and without the search, with scenarios and the self-check.
#
# Usage: dev/sanitize-cpp.sh
# Prints the number of states and a checksum of the statuses; any sanitizer
# report or a non-zero exit is a problem. It doesn't replace the sanitizer
# builds of the whole package (dev/cpp-standings-engine.md).
set -eu
cd "$(dirname "$0")/.."

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

Rscript dev/sanitize/export-states.R "$tmp/states.txt"
engine=$(ls src/*.cpp | grep -v -e bindings.cpp -e cpp11.cpp)
# shellcheck disable=SC2086
${CXX:-clang++} -std=c++20 -O1 -g -fsanitize=address,undefined \
  -fno-sanitize-recover=undefined -fno-omit-frame-pointer -Wall -Wextra \
  -Isrc $engine dev/sanitize/driver.cpp -o "$tmp/driver"
"$tmp/driver" "$tmp/states.txt"
