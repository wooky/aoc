import std/re, std/strutils, std/tables, zero_functional, ../aoc

type Disc = ref object of RootObj
  weight: int
  holding: seq[string]

  subWeight: int

type DiscList = Table[string, Disc]
type TotalWeightCalculationResult = tuple[weight: int, adjust: int]

proc calculateTotalWeight(disc: Disc, discs: DiscList): TotalWeightCalculationResult =
  var weights = newTable[int, seq[Disc]]()
  for held in disc.holding:
    let nextDisc = discs[held]
    let res = nextDisc.calculateTotalWeight(discs)
    if res.adjust != 0:
      return (weight: 0, adjust: res.adjust)
    if res.weight in weights:
      weights[res.weight] &= nextDisc
    else:
      weights[res.weight] = @[nextDisc]
    disc.subWeight += res.weight
  
  var singleWeightDisc: Disc = nil
  var multiWeightDisc: int = 0
  for k, v in weights.pairs:
    if v.len() == 1:
      singleWeightDisc = v[0]
    else:
      multiWeightDisc = k
  
  if singleWeightDisc != nil and multiWeightDisc != 0:
    return (weight: 0, adjust: (multiWeightDisc - singleWeightDisc.subWeight).abs())

  return (weight: disc.weight + disc.subWeight, adjust: 0)

proc parseToDisc(line: string): (string, Disc) =
  var tokens: array[3, string]
  discard line.find(re"(\w+) \((\d+)\)(?: -> (.+))?", tokens)
  (
    tokens[0],
    Disc(
      weight: tokens[1].parseInt(),
      holding: tokens[2].split(", ") --> filter(it != ""),
      subWeight: 0,
    )
  )

proc day07*(file: string): Solution =
  let discsSeq =
    file.splitLines() -->
      filter(it != "") -->
      map(it.parseToDisc)
  let discs: DiscList = discsSeq.toTable

  var idx = 0
  var rootDisc: string
  while idx != -1:
    rootDisc = discsSeq[idx][0]
    idx = discsSeq --> index(rootDisc in it[1].holding)

  let (_, adjust) = discs[rootDisc].calculateTotalWeight(discs)

  Solution(s1: rootDisc, s2: $adjust)
