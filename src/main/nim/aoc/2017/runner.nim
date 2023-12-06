import ../aoc
import day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day24, day25

proc run2017*(file: string, day: int): Solution =
  case day
  of 1: day01(file)
  of 2: day02(file)
  of 3: day03(file)
  of 4: day04(file)
  of 5: day05(file)
  of 6: day06(file)
  of 7: day07(file)
  of 8: day08(file)
  of 9: day09(file)
  of 10: day10(file)
  of 11: day11(file)
  of 12: day12(file)
  of 13: day13(file)
  of 14: day14(file)
  of 15: day15(file)
  of 16: day16(file)
  of 17: day17(file)
  of 18: day18(file)
  of 19: day19(file)
  of 20: day20(file)
  of 21: day21(file)
  of 22: day22(file)
  of 24: day24(file)
  of 25: day25(file)
  else: raise newException(ValueError, "Invalid day")
