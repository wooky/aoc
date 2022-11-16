import knot_hasher, std/strutils, ../aoc

proc day10*(file: string): Solution =
  let s1 = block:
    var hasher = newKnotHasher()
    for size in file.split(","):
      hasher.feed(size.parseInt())
    hasher.chain[0] * hasher.chain[1]

  let s2 = file.sparseHash()

  Solution(s1: $s1, s2: s2)
