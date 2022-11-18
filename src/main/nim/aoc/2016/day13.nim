import std/bitops, std/sets, std/strutils, std/tables, zero_functional, ../aoc

type
  Coord = tuple[x: int, y: int]
  Node = tuple[coord: Coord, dist: int]
  Stats = tuple[coord: Coord, gScore: int]

const
  start: Coord = (1, 1)
  goal: Coord = (31, 39)

proc fScore(stats: Stats): int = 
  stats.gScore + abs(stats.coord.x - goal.x) + abs(stats.coord.y - goal.y)

proc `<`(a, b: (Coord, Stats)): bool = a[1].fScore() < b[1].fScore()

proc isSpace(coord: Coord, input: int, spaces: var Table[Coord, bool]): bool =
  if coord notin spaces:
    spaces[coord] =
      (coord.x * coord.x + 3 * coord.x + 2 * coord.x * coord.y + coord.y + coord.y * coord.y + input)
        .countSetBits() %% 2 == 0
  spaces[coord]

proc aStar(openMap: var Table[Coord, Stats], closedSet: var HashSet[Coord], input: int, spaces: var Table[Coord, bool]): int =
  while true:
    let node = block: # hacky stuff to get around the compiler complaining
      let openMapCopy = openMap
      openMapCopy.pairs --> min()
    let coord = node[0]
    if coord == goal:
      return node[1].gScore

    let tentativeGscore = node[1].gScore + 1
    openMap.del(coord)
    closedSet.incl(coord)

    @[
      (x: coord.x - 1, y: coord.y),
      (x: coord.x + 1, y: coord.y),
      (x: coord.x, y: coord.y - 1),
      (x: coord.x, y: coord.y + 1),
    ] -->
      filter(
        it notin closedSet and it.x >= 0 and it.y >= 0 and
          (it notin openMap or openMap[it].gScore > tentativeGscore) and
          it.isSpace(input, spaces)
      ) -->
      foreach(
        block: openMap[it] = (it, tentativeGscore)
      )

proc day13*(file: string): Solution =
  let inputInt = file.parseInt()
  var closedSet = @[start].toHashSet()
  var openMap: Table[Coord, Stats]
  var queue: seq[Node] = @[(coord: start, dist: 0)]
  var spaces: Table[Coord, bool]

  while queue.len() != 0:
    let node = queue[0]
    queue.delete(0)
    let coord = node.coord
    closedSet.incl(coord)
    if node.dist == 50:
      openMap[coord] = (coord: coord, gscore: 50)
      continue

    @[
      (x: coord.x - 1, y: coord.y),
      (x: coord.x + 1, y: coord.y),
      (x: coord.x, y: coord.y - 1),
      (x: coord.x, y: coord.y + 1)
    ] -->
      filter(it notin closedSet and it.x >= 0 and it.y >= 0 and it.isSpace(inputInt, spaces)) -->
      foreach(block:
        closedSet.incl(it)
        queue &= (coord: it, dist: node.dist + 1)
      )

  let visited = closedSet.len()
  let leastSteps = aStar(openMap, closedSet, inputInt, spaces)
  Solution(s1: $leastSteps, s2: $visited)
