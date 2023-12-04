#!/bin/bash
set -e

AOC_ROOT=$(pwd)
cd $AOC_ROOT/src/main/ada && gprbuild
cd $AOC_ROOT/src/main/nim && nimble install -y && nimble lib
cd $AOC_ROOT/src/main/zig && zig build
cd $AOC_ROOT/src/main/cxx && g++ -std=c++20 -shared -fPIC -fpic -o $AOC_ROOT/build/libaoc_cxx.so runner.cpp 2023/runner.cpp 2023/day01.cpp 2023/day02.cpp
