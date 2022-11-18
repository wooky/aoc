import std/md5, std/re, std/strutils, std/tables, ../aoc
# import std/locks, std/threadpool
# {.experimental.}

type
  Pad = object of RootObj
    input: string
    repeat: int
    seqs: OrderedTable[int, string]
    # seqsLock: Lock
    keysGenerated: int
    finalIndex: int

proc processHash(pad: ptr Pad, i: int) =
  let triple = re"(.)\1\1"
  var key = pad.input & i.intToStr()
  for _ in 1 .. pad.repeat:
    key = key.getMD5()
  let tripleIdx = key.find(triple)
  if tripleIdx != -1:
    # withLock pad.seqsLock:
      for k, v in pad.seqs:
        if k in i - 999 .. i and v in key:
          if pad.keysGenerated == 63:
            pad.finalIndex = k
            return
          pad.keysGenerated += 1
      pad.seqs[i] = key[tripleIdx].repeat(5)

# TODO parallelizing this actually takes longer!
proc oneTimePad(input: string, repeat: int): int =
  var pad = Pad(
    input: input,
    repeat: repeat,
    finalIndex: 0,
  )

  var i = 0
  while pad.finalIndex == 0:
    # spawnX processHash(pad.addr, i)
    processHash(pad.addr, i)
    i += 1
    
  pad.finalIndex

proc day14*(file: string): Solution =
  Solution(
    s1: $oneTimePad(file, 1),
    s2: $oneTimePad(file, 2017),
  )
