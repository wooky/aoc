import ../aoc
import day04, day07, day09, day10, day13, day14, day15, day16, day17, day18, day19

proc run2016*(file: string, day: int): Solution =
  case day
  of 4: day04(file)
  of 7: day07(file)
  of 9: day09(file)
  of 10: day10(file)
  of 13: day13(file)
  of 14: day14(file)
  of 15: day15(file)
  of 16: day16(file)
  of 17: day17(file)
  of 18: day18(file)
  of 19: day19(file)
  else: raise newException(ValueError, "Invalid day")
