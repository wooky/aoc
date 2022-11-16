import knot_hasher, std/options, std/sets, std/strformat, std/strutils, zero_functional, ../aoc

type
  Coord = tuple[row: int, col: int]

proc popOrNil(s: var HashSet[Coord], coord: Coord, dRow: int, dCol: int): Option[Coord] =
  let target: Coord = (row: coord.row + dRow, col: coord.col + dCol)
  if not s.missingOrExcl(target):
    return some(target)

proc day14*(file: string): Solution =
  var used = initHashSet[Coord]()
  (0 .. 127) -->
    map(row = it) -->
    map(fmt"{file}-{row}".sparseHash()) -->
    foreach(block:
      it -->
        map(($it).parseHexInt().toBin(4)) -->
        flatten() -->
        enumerate() -->
        filter(it[1] == '1') -->
        foreach(used.incl((row, it[0])))
    )

  var regions = 0
  while used.len() != 0:
    regions += 1
    var queue = @[used.pop()]
    while queue.len() != 0:
      let coord = queue.pop()
      @[
        used.popOrNil(coord, -1, 0),
        used.popOrNil(coord, 0, -1),
        used.popOrNil(coord, 0, 1),
        used.popOrNil(coord, 1, 0),
      ] -->
        filter(it.isSome()) -->
        foreach(queue &= it.get())

  Solution(s1: $used.len(), s2: $regions)
