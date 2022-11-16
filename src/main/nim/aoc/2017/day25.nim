import std/sets, std/strutils, std/tables, ../aoc

type
  State = array[2, StateCommand]
  StateCommand = object
    write: bool
    move: int
    nextState: char

proc day25*(file: string): Solution =
  let lines = file.splitLines()

  var state = lines[0][^2]
  var steps = lines[1].split(' ')[^2].parseInt()

  var states: Table[char, State]
  for idx in countup(3, lines.len() - 1, 10):
    let stateName = lines[idx][^2]
    var stateCommands: State
    for stateCommandIdx in 0 .. 1:
      stateCommands[stateCommandIdx] = StateCommand(
        write: lines[idx + 2 + stateCommandIdx*4][^2] == '1',
        move: if lines[idx + 3 + stateCommandIdx*4][^6 .. ^2] == "right": 1 else: -1,
        nextState: lines[idx + 4 + stateCommandIdx*4][^2],
      )
    states[stateName] = stateCommands

  var tape: HashSet[int]
  var cursor = 0
  for _ in 1 .. steps:
    let currValue = if cursor in tape: 1 else: 0
    let stateCommand = states[state][currValue]
    if stateCommand.write:
      tape.incl(cursor)
    else:
      tape.excl(cursor)
    cursor += stateCommand.move
    state = stateCommand.nextState
  
  Solution(s1: $tape.len(), s2: "XXX")
