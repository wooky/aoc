import std/strutils, zero_functional, ../aoc

proc countSteps(instructions: seq[int], deltaThreeOrMore: int): int =
  var ins = instructions
  var steps = 0
  var idx = 0
  while idx >= 0 and idx < ins.len():
    let delta = ins[idx]
    ins[idx] += (if delta >= 3: deltaThreeOrMore else: 1)
    steps += 1
    idx += delta
  return steps

proc day05*(file: string): Solution =
  var instructions =
    file.splitLines() -->
      filter(it != "") -->
      map(it.parseInt()) -->
      to(seq)

  Solution(
    s1: $countSteps(instructions, 1),
    s2: $countSteps(instructions, -1),
  )
