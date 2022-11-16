import std/sequtils, std/strutils, zero_functional, ../aoc

type Banks = seq[int]
proc redistribute(banks: var Banks): int =
  var banksSeen = newSeq[Banks]()
  while banks notin banksSeen:
    banksSeen &= banks
    var idx = banks.maxIndex()
    var blocksRemaining = banks[idx]
    banks[idx] = 0
    while blocksRemaining > 0:
      idx = (idx + 1) %% banks.len
      banks[idx] += 1
      blocksRemaining -= 1
  return banksSeen.len()

proc day06*(file: string): Solution =
  var banks: Banks = file.splitWhitespace() --> map(parseInt(it))
  let s1 = redistribute(banks)
  let s2 = redistribute(banks)
  Solution(s1: $s1, s2: $s2)
