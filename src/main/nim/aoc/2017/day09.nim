import ../aoc

proc day09*(file: string): Solution =
  var totalScore = 0
  var garbageCount = 0
  var groupDepth = 0
  var readingGarbage = false
  var ignoreNext = false

  for c in file:
    if ignoreNext:
      ignoreNext = false
    elif c == '!':
      ignoreNext = true
    elif readingGarbage:
      if c == '>':
        readingGarbage = false
      else:
        garbageCount += 1
    elif c == '<':
      readingGarbage = true
    elif c == '{':
      groupDepth += 1
    elif c == '}':
      totalScore += groupDepth
      groupDepth -= 1

  assert(groupDepth == 0)
  Solution(s1: $totalScore, s2: $garbageCount)
