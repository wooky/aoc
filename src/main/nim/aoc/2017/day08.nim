import std/re, std/strutils, std/tables, zero_functional, ../aoc

type
  Opcode = enum
    inc,
    dec,

  Comparison = enum
    ge = ">=",
    gt = ">",
    eq = "==",
    le = "<=",
    lt = "<",
    ne = "!="

proc day08*(file: string): Solution =
  let registers = newTable[string, int]()
  var highestValueEver = 0

  for line in file.splitLines():
    if line == "":
      continue
    var tokens: array[6, string]
    discard line.find(re"(\w+) (\w+) (-?\d+) if (\w+) ([<>=!]+) (-?\d+)", tokens)
    let targetRegister = tokens[0]
    let opcode = parseEnum[Opcode](tokens[1])
    let delta = tokens[2].parseInt()
    let comparisonRegister = tokens[3]
    let comparisonSymbol = parseEnum[Comparison](tokens[4])
    let comparisonRight = tokens[5].parseInt()

    let comparisonLeft = registers.getOrDefault(comparisonRegister, 0)
    let doOp =
      case comparisonSymbol
      of ge: comparisonLeft >= comparisonRight
      of gt: comparisonLeft > comparisonRight
      of eq: comparisonLeft == comparisonRight
      of le: comparisonLeft <= comparisonRight
      of lt: comparisonLeft < comparisonRight
      of ne: comparisonLeft != comparisonRight
    if doOp:
      let actualDelta =
        case opcode
        of inc: delta
        of dec: -delta
      let targetValue = registers.getOrDefault(targetRegister, 0) + actualDelta
      registers[targetRegister] = targetValue
      highestValueEver = max(highestValueEver, targetValue)

  let maxRegister = registers.values() --> max()
  
  Solution(s1: $maxRegister, s2: $highestValueEver)
