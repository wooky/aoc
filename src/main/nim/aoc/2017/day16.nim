import std/re, std/strutils, ../aoc

type
  OperationKind = enum
    spin,
    exchange,
    partner,
  Operation = ref OperationObj
  OperationObj = object
    case kind: OperationKind
    of spin:
      size: int
    of exchange:
      posA, posB: int
    of partner:
      progA, progB: char

proc run(op: Operation, programs: var string) =
  case op.kind
  of spin:
    programs = programs[^op.size .. ^1] & programs[0 ..< ^op.size]
  of exchange:
    swap(programs[op.posA], programs[op.posB])
  of partner:
    swap(programs[programs.find(op.progA)], programs[programs.find(op.progB)])

proc day16*(file: string): Solution =
  var operations = newSeq[Operation]()
  for move in file.split(","):
    var tokens: array[3, string]
    discard move.find(re"(\w)([\w\d]+)(?:\/([\w\d]+))?", tokens)
    let op =
      case tokens[0]
      of "s":
        let size = tokens[1].parseInt()
        Operation(kind: spin, size: size)
      of "x":
        let posA = tokens[1].parseInt()
        let posB = tokens[2].parseInt()
        Operation(kind: exchange, posA: posA, posB: posB)
      of "p":
        let progA = tokens[1][0]
        let progB = tokens[2][0]
        Operation(kind: partner, progA: progA, progB: progB)
      else:
        raise newException(ValueError, "Invalid operation " & tokens[0])
    operations &= op

  var programs = ""
  for c in 'a' .. 'p': programs &= c
  var positions = @[programs]

  while true:
    for op in operations:
      op.run(programs)
    if programs == positions[0]:
      break
    positions &= programs
  let firstPosition = positions[1]
  let billionthPosition = positions[1_000_000_000 %% positions.len()]

  Solution(s1: firstPosition, s2: billionthPosition)
