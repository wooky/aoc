#!/bin/bash
set -e

AOC_ROOT=$(pwd)
cd $AOC_ROOT/src/main/ada && gprbuild
cd $AOC_ROOT/src/main/nim && nimble install -y && nimble lib
cd $AOC_ROOT/src/main/zig && zig build
