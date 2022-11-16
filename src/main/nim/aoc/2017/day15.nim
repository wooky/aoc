import std/bitops, std/strutils, std/threadpool, ../aoc
{.experimental: "parallel".}

const
  FactorA = 16807
  FactorB = 48271
type
  Judge = ref object of RootObj
    initA: int
    initB: int

  Generator = ref object of RootObj
    factor: int
    prevValue: int
    divisibleBy: int

proc extractInitialValue(line: string): int =
  let space = line.rfind(' ')
  line[space + 1 .. ^1].parseInt()

proc run(generator: Generator): int =
  while true:
    generator.prevValue = (generator.prevValue * generator.factor) %% 2147483647
    if generator.prevValue %% generator.divisibleBy == 0:
      return generator.prevValue.bitand(0xffff)

proc run(judge: Judge, steps: int, divA: int, divB: int): int =
  let genA = Generator(factor: FactorA, prevValue: judge.initA, divisibleBy: divA)
  let genB = Generator(factor: FactorB, prevValue: judge.initB, divisibleBy: divB)
  var finalCount = 0
  for _ in 1 .. steps:
    if genA.run() == genB.run():
      finalCount += 1
  finalCount

proc day15*(file: string): Solution =
  let lines = file.splitLines()
  let judge = Judge(
    initA: lines[0].extractInitialValue(),
    initB: lines[1].extractInitialValue(),
  )

  var responses: array[2, int]
  parallel:
    responses[0] = spawn judge.run(40_000_000, 1, 1)
    responses[1] = spawn judge.run(5_000_000, 4, 8)
  
  Solution(s1: $responses[0], s2: $responses[1])
