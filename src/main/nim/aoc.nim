import aoc/aoc, "aoc/2017/runner", genny

type
  ExportSolution* = object
    s1*: cstring
    s2*: cstring

proc run*(file: string, year: int, day: int): ExportSolution =
  let solution =
    try:
      case year
      of 2017: run2017(file, day)
      else: raise newException(ValueError, "Invalid year")
    except ValueError as e:
      quit(e.msg)
  ExportSolution(s1: solution.s1.cstring, s2: solution.s2.cstring)

exportProcs:
  run

exportObject ExportSolution:
  discard

writeFiles("build", "aoc_nim")
include ../../../build/internal
