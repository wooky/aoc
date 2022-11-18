import std/strutils, ../aoc

type
  Disc = object
    initial: int
    positions: int

proc timing(discs: seq[Disc]): int =
  for t in 0 .. high(int):
    block attempt:
      for disc in discs:
        if (disc.initial + t) %% disc.positions != 0:
          break attempt
      return t

proc day15*(file: string): Solution =
  var discs = newSeq[Disc]()
  for line in file.splitLines():
    if line == "":
      continue
    let tokens = line.splitWhitespace()
    let id = tokens[1][1 .. ^1].parseInt()
    let positions = tokens[3].parseInt()
    let initialPosition = tokens[11][0 .. ^2].parseInt()
    discs &= Disc(
      initial: id + initialPosition,
      positions: positions,
    )
  let s1 = discs.timing()

  discs &= Disc(initial: discs.len() + 1, positions: 11)
  let s2 = discs.timing()

  Solution(s1: $s1, s2: $s2)
