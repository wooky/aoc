import std/strutils, std/tables, ../aoc

type
  Coord = tuple[x: int, y: int]
  Grid = TableRef[Coord, int]
  CalculationResult = tuple[last: int, distance: int]
  Delta = enum
    up,
    down,
    left,
    right

proc calculateSpiralGridDistance(limit: int, nextOp: proc (grid: Grid, last: int, coord: Coord): int): CalculationResult =
  var grid: Grid = newTable[Coord, int]()
  var coord: Coord = (0, 0)
  var delta = Delta.right
  var size, remain = 1
  var curr = 1

  while curr < limit:
    grid[coord] = curr

    if remain == 0:
      (delta, size) =
        case delta
        of up: (left, size + 1)
        of down: (right, size + 1)
        of left: (down, size)
        of right: (up, size)
      remain = size
    remain -= 1

    case delta
    of up: coord.y -= 1
    of down: coord.y += 1
    of left: coord.x -= 1
    of right: coord.x += 1

    curr = nextOp(grid, curr, coord)
  
  (curr, coord.x.abs() + coord.y.abs())

proc day03*(file: string): Solution =
  let limit = file.parseInt()

  let s1 = calculateSpiralGridDistance(
    limit,
    proc (grid: Grid, last: int, coord: Coord): int =
      last + 1
  ).distance

  let s2 = calculateSpiralGridDistance(
    limit,
    proc (grid: Grid, last: int, coord: Coord): int =
      grid.getOrDefault((coord.x - 1, coord.y - 1), 0) +
      grid.getOrDefault((coord.x - 1, coord.y), 0) +
      grid.getOrDefault((coord.x - 1, coord.y + 1), 0) +
      grid.getOrDefault((coord.x, coord.y - 1), 0) +
      grid.getOrDefault((coord.x, coord.y + 1), 0) +
      grid.getOrDefault((coord.x + 1, coord.y - 1), 0) +
      grid.getOrDefault((coord.x + 1, coord.y), 0) +
      grid.getOrDefault((coord.x + 1, coord.y + 1), 0)
  ).last

  Solution(s1: $s1, s2: $s2)
