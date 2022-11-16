import std/strutils, ../aoc

proc day17*(file: string): Solution =
  let steps = file.parseInt()

  let s1 = block:
    var idx = 0
    var buf = @[0]
    for val in 1 .. 2017:
      idx = (idx + steps) %% val + 1
      buf.insert(val, idx)
    buf[idx + 1]

  let s2 = block:
    var idx = 0
    var after0: int
    for val in 1 .. 50000000:
      idx = (idx + steps) %% val + 1
      if idx == 1:
        after0 = val
    after0

  Solution(s1: $s1, s2: $s2)
