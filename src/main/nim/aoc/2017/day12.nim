import std/strutils, std/tables, zero_functional, ../aoc

proc countInGroup(comms: var TableRef[int, seq[int]], programId: int): int =
  var inGroup = 0
  var queue = @[programId]
  while queue.len() != 0:
    let curr = queue.pop()
    var comm: seq[int]
    if comms.pop(curr, comm):
      inGroup += 1
      queue &= comm
  inGroup

proc day12*(file: string): Solution =
  var comms = newTable[int, seq[int]]()
  for line in file.splitLines():
    if line == "":
      continue
    let tokens = line.split(" <-> ")
    comms[tokens[0].parseInt()] = tokens[1].split(", ") --> map(it.parseInt())

  let withProg0 = comms.countInGroup(0)

  var groups = 1
  while comms.len() != 0:
    for programId in comms.keys():
      discard comms.countInGroup(programId)
      groups += 1
      break

  Solution(s1: $withProg0, s2: $groups)
