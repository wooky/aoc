import std/re, std/strutils, std/tables, zero_functional, ../aoc

type
  Room = object
    encryptedName: string
    sectorId: int

proc sortByCountThenChar(x, y: (char, seq[char])): int =
  result = cmp(y[1].len(), x[1].len())
  if result == 0:
    result = cmp(x[0], y[0])

proc day04*(file: string): Solution =
  var realRooms = newSeq[Room]()
  for line in file.splitLines():
    if line == "":
      continue

    var tokens: array[3, string]
    discard line.find(re"(.*?)-(\d+)\[(\w{5})\]", tokens)
    let expectedChecksum = tokens[2]
    let encryptedName = tokens[0] --> filter(it != '-')
    var charsToOccurs = encryptedName --> group(it)
    charsToOccurs.sort(sortByCountThenChar)
    var actualChecksum = ""
    for c in charsToOccurs.keys:
      actualChecksum &= c
      if actualChecksum.len() == 5:
        break
    if expectedChecksum == actualChecksum:
      realRooms &= Room(
        encryptedName: encryptedName --> fold("", a & it),
        sectorId: tokens[1].parseInt(),
      )

  let realRoomSum =
    realRooms -->
      map(it.sectorId) -->
      sum()

  let northPoleObjectsStorageIdx =
    realRooms -->
      map(room = it) -->
      map(room.encryptedName -->
        map(char((int(it) - int('a') + room.sectorId) %% 26 + int('a')))
      ) -->
      index(it == "northpoleobjectstorage")
  let northPoleObjectsStorage = realRooms[northPoleObjectsStorageIdx].sectorId
  
  Solution(s1: $realRoomSum, s2: $northPoleObjectsStorage)
