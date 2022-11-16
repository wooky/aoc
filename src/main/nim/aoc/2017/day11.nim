import std/strutils, ../aoc

type
  HexCoord = enum
    nw,
    n,
    ne,
    sw,
    s,
    se,

proc day11*(file: string): Solution =
  var x, y = 0
  var currDistance, furthestDistance = 0
  for token in file.split(","):
    let coord = parseEnum[HexCoord](token)
    case coord
    of nw: x -= 1; y -= 1
    of n: y -= 2
    of ne: x += 1; y -= 1
    of sw: x -= 1; y += 1
    of s: y += 2
    of se: x += 1; y += 1

    currDistance = (x.abs() + y.abs()) /% 2
    furthestDistance = max(furthestDistance, currDistance)

  Solution(s1: $currDistance, s2: $furthestDistance)
