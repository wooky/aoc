# Package

version       = "0.0.0"
author        = "Yakov Lipkovich"
description   = "Advent of Code solutions"
license       = "Unlicense"


# Dependencies

requires "nim >= 1.6.0"
requires "zero_functional >= 1.3.0"
requires "pixie >= 5.0.0"

task lib, "Generate library":

  proc compile(libName: string, flags = "") =
    exec "nim c " & flags & " --app:lib --gc:orc --threads:on --out:" & libName & " --outdir:../../../build aoc.nim"

  compile "libaoc_nim.so"
