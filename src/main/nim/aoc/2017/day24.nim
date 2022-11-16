import std/sets, std/strutils, std/tables, zero_functional, ../aoc

type
  BridgeResult = object
    overallStrongest: int
    longest: int
    strongestLongest: int

proc traverse(components: Table[int, HashSet[int]], input: int): BridgeResult =
  result = BridgeResult(
    overallStrongest: input,
    longest: 1,
    strongestLongest: input,
  )
  for nextInput in components[input].items():
    var newComponents = components
    newComponents[input].excl(nextInput)
    newComponents[nextInput].excl(input)
    let nextResult = newComponents.traverse(nextInput) # TODO parallelize
    let nextOverallStrongest = input * 2 + nextResult.overallStrongest
    result.overallStrongest = max(result.overallStrongest, nextOverallStrongest)
    let nextLongest = nextResult.longest + 1
    let nextStrongestLongest = input * 2 + nextResult.strongestLongest
    if nextLongest > result.longest:
      result.longest = nextLongest
      result.strongestLongest = nextStrongestLongest
    elif nextLongest == result.longest:
      result.strongestLongest = max(result.strongestLongest, nextStrongestLongest)

proc day24*(file: string): Solution =
  var components = initTable[int, HashSet[int]]()
  for line in file.splitLines():
    if line == "":
      continue
    let tokens = line.split('/')
    let p0 = tokens[0].parseInt()
    let p1 = tokens[1].parseInt()
    components.mgetOrPut(p0, initHashSet[int]()).incl(p1)
    components.mgetOrPut(p1, initHashSet[int]()).incl(p0)

  let bridgeResult = components.traverse(0)
  Solution(s1: $bridgeResult.overallStrongest, s2: $bridgeResult.strongestLongest)
