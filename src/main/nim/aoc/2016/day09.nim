import ../aoc

type
  LengthAndIndex = tuple[length: int, index: int]
  Stage = enum
    idle
    length
    repeat

proc decompress(input: string, initialIndex: int, maxLength: int, recursive: bool): LengthAndIndex =
  var stage = Stage.idle
  var index = initialIndex
  var totalLength = 0
  var length = 0
  var repeat = 0

  while (index < initialIndex + maxLength):
    let here = input[index]
    case stage
    of Stage.idle:
      case here
      of '(': stage = Stage.length
      else: totalLength += 1
    of Stage.length:
      case here
      of 'x': stage = Stage.repeat
      else: length = length * 10 + (int(here) - int('0'))
    of Stage.repeat:
      case here
      of ')':
        if recursive:
          let decompressedChunk = decompress(input, index + 1, length, true)
          totalLength += decompressedChunk.length * repeat
          index = decompressedChunk.index - 1
        else:
          totalLength += length * repeat
          index += length
        length = 0
        repeat = 0
        stage = Stage.idle
      else: repeat = repeat * 10 + (int(here) - int('0'))
    index += 1

  (totalLength, index)

proc day09*(file: string): Solution =
  Solution(
    s1: $decompress(file, 0, file.len(), false).length,
    s2: $decompress(file, 0, file.len(), true).length,
  )
