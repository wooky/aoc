import std/sets, std/strutils, std/tables, zero_functional, ../aoc

type
  DepotKind = enum
    otherBot
    output
  Depot = object
    kind: DepotKind
    id: string
  Bot = object
    id: string
    chips: HashSet[int]
    lowDepot: Depot
    highDepot: Depot

proc deposit(
  depot: Depot,
  chip: int,
  bots: var Table[string, Bot],
  queue: var seq[ptr Bot],
  outputs: var Table[string, int]
) =
  case depot.kind
    of otherBot:
      let bot = bots[depot.id].addr
      bots[depot.id].chips.incl(chip)
      if (bots[depot.id].chips.len() == 2):
        queue &= bot
    of output:
      outputs[depot.id] = chip

proc day10*(file: string): Solution =
  var bots: Table[string, Bot]
  var queue = newSeq[ptr Bot]()
  var outputs: Table[string, int]
  for line in file.splitLines():
    if line == "":
      continue
    let cmd = line.splitWhitespace()
    case cmd[0]
    of "bot":
      let id = cmd[1]
      let bot = bots.mgetOrPut(id, Bot(id: id)).addr
      bot.lowDepot = Depot(
        kind: if cmd[5] == "output": output else: otherBot,
        id: cmd[6],
      )
      bot.highDepot = Depot(
        kind: if cmd[10] == "output": output else: otherBot,
        id: cmd[11],
      )
    of "value":
      let id = cmd[5]
      let bot = bots.mgetOrPut(id, Bot(id: id)).addr
      bot.chips.incl(cmd[1].parseInt())
      if bot.chips.len() == 2:
        queue &= bot
    else: raise newException(ValueError, "Invalid command type " & cmd[0])

  let expected = [17, 61].toHashSet()
  var expectedBotId = ""
  while queue.len() != 0:
    var bot = queue.pop()
    if bot.chips == expected:
      expectedBotId = bot.id

    bot.lowDepot.deposit(bot.chips --> min(), bots, queue, outputs)
    bot.highDepot.deposit(bot.chips --> max(), bots, queue, outputs)
    bot.chips.clear()

  Solution(
    s1: expectedBotId,
    s2: $(outputs["0"] * outputs["1"] * outputs["2"]),
  )
