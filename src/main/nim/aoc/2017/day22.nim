import std/strutils, std/tables, zero_functional, ../aoc

type
  Coord = tuple[row: int, col: int]
  NodeState = enum
    infected,
    weakened,
    flagged,

proc day22*(file: string): Solution =
  var infectedNodes = initTable[Coord, NodeState]()
  let initialCoord = block:
    var width: int
    var height = 0
    for line in file.splitLines():
      if line == "":
        continue
      line -->
        enumerate() -->
        filter(it.elem == '#') -->
        foreach(block: infectedNodes[(row: height, col: it.idx)] = NodeState.infected)
      width = line.len()
      height += 1
    
    (row: height /% 2, col: width /% 2)

  let s1 = block:
    var uncleanNodes = infectedNodes
    var curr = initialCoord
    var dir = (row: -1, col: 0)
    var infectionBursts = 0
    for _ in 1 .. 10000:
      var unused: NodeState
      if uncleanNodes.pop(curr, unused):
        dir = (row: dir.col, col: -dir.row)
      else:
        dir = (row: -dir.col, col: dir.row)
        uncleanNodes[curr] = NodeState.infected
        infectionBursts += 1
      
      curr.row += dir.row
      curr.col += dir.col
    infectionBursts

  let s2 = block:
    var uncleanNodes = infectedNodes
    var curr = initialCoord
    var dir = (row: -1, col: 0)
    var infectionBursts = 0
    for _ in 1 .. 10000000:
      if curr in uncleanNodes:
        case uncleanNodes[curr]
        of weakened:
          uncleanNodes[curr] = NodeState.infected
          infectionBursts += 1
        of infected:
          uncleanNodes[curr] = NodeState.flagged
          dir = (row: dir.col, col: -dir.row)
        of flagged:
          uncleanNodes.del(curr)
          dir = (row: -dir.row, col: -dir.col)
      else:
        uncleanNodes[curr] = NodeState.weakened
        dir = (row: -dir.col, col: dir.row)
      
      curr.row += dir.row
      curr.col += dir.col
    infectionBursts

  Solution(s1: $s1, s2: $s2)
