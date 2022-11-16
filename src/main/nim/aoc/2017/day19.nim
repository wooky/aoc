import std/strutils, ../aoc

type
  Direction = enum
    up,
    down,
    left,
    right,

proc day19*(file: string): Solution =
  let diagram = file.splitLines()

  var col = diagram[0].find('|')
  var row = 0
  var direction = Direction.down
  var letters = ""
  var steps = 0
  while true:
    case diagram[row][col]
    of ' ':
      break
    of '|', '-':
      discard
    of 'A' .. 'Z':
      letters &= diagram[row][col]
    of '+':
      direction =
        if direction != down and diagram[row - 1][col] != ' ': up
        elif direction != up and diagram[row + 1][col] != ' ': down
        elif direction != right and diagram[row][col - 1] != ' ': left
        elif direction != left and diagram[row][col + 1] != ' ': right
        else: raise newException(ValueError, "Don't know where to go from row " & $row & " col " & $col)
    else:
      raise newException(ValueError, "Unexpected symbol " & $diagram[row][col] & " at row " & $row & " col " & $col)

    case direction
    of up: row -= 1
    of down: row += 1
    of left: col -= 1
    of right: col += 1

    steps += 1

  Solution(s1: letters, s2: $steps)
