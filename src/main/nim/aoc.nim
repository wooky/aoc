import aoc/aoc, "aoc/2016/runner", "aoc/2017/runner", genny, pixie

type
  ExportSolution* = object
    s1*: cstring
    s2*: cstring

proc run*(file: string, year: int, day: int): ExportSolution =
  let solution =
    try:
      case year
      of 2016: run2016(file, day)
      of 2017: run2017(file, day)
      else: raise newException(ValueError, "Invalid year")
    except ValueError as e:
      echo e.getStackTrace()
      quit(e.msg)
    except PixieError as e:
      echo e.getStackTrace()
      quit(e.msg)
  ExportSolution(s1: solution.s1.cstring, s2: solution.s2.cstring)

exportProcs:
  run

exportObject ExportSolution:
  discard

writeFiles("build", "aoc_nim")
include ../../../build/internal
