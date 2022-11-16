import std/strutils, std/tables, ../aoc

type
  RegisterTable = Table[char, int]
  RegisterOrValueKind = enum
    reg,
    val,
  RegisterOrValue = ref RegisterOrValueObj
  RegisterOrValueObj = object
    case kind: RegisterOrValueKind
    of reg:
      reg: char
    of val:
      val: int

  Instructions = seq[Instruction]
  InstructionKind = enum
    sndx = "snd",
    setxy = "set",
    addxy = "add",
    mulxy = "mul",
    modxy = "mod",
    rcvx = "rcv",
    jgzxy = "jgz",
  Instruction = ref InstructionObj
  InstructionObj = object
    case kind: InstructionKind
    of sndx, rcvx:
      xq: char
    of jgzxy:
      xjgzrov: RegisterOrValue
      yjgzrov: RegisterOrValue
    else:
      x: char
      yrov: RegisterOrValue

  Program = object
    instructions: Instructions
    registers: RegisterTable
    pc: int
    sends: int
    inQueue: ref seq[int]
    outQueue: ref seq[int]

proc toRegisterOrValue(s: string): RegisterOrValue =
  if s[0] in 'a' .. 'z': RegisterOrValue(kind: reg, reg: s[0])
  else: RegisterOrValue(kind: val, val: s.parseInt())

proc value(rov: RegisterOrValue, registers: RegisterTable): int =
  case rov.kind
  of reg: registers[rov.reg]
  of val: rov.val

proc newProgram(
  instructions: Instructions,
  id: int,
  inQueue: ref seq[int],
  outQueue: ref seq[int],
): Program =
  Program(
    instructions: instructions,
    registers: {'p': id}.toTable,
    pc: 0,
    sends: 0,
    inQueue: inQueue,
    outQueue: outQueue,
  )

proc exec(program: var Program) =
  while true:
    let instruction = program.instructions[program.pc]
    case instruction.kind
    of sndx:
      program.sends += 1
      program.outQueue[] &= program.registers[instruction.xq]
    of setxy:
      program.registers[instruction.x] = instruction.yrov.value(program.registers)
    of addxy:
      program.registers.mgetOrPut(instruction.x, 0) += instruction.yrov.value(program.registers)
    of mulxy:
      program.registers.mgetOrPut(instruction.x, 0) *= instruction.yrov.value(program.registers)
    of modxy:
      program.registers[instruction.x] = program.registers.mgetOrPut(instruction.x, 0) %% instruction.yrov.value(program.registers)
    of rcvx:
      if program.inQueue[].len() == 0:
        return
      program.registers[instruction.xq] = program.inQueue[0]
      program.inQueue[].delete(0)
    of jgzxy:
      if instruction.xjgzrov.value(program.registers) > 0:
        program.pc += instruction.yjgzrov.value(program.registers) - 1
    
    program.pc += 1

proc day18*(file: string): Solution =
  var instructions = newSeq[Instruction]()
  for line in file.splitLines():
    if line == "":
      continue
    let tokens = line.splitWhitespace()
    let opcode = parseEnum[InstructionKind](tokens[0])
    let instruction =
      case opcode
      of sndx, rcvx:
        Instruction(
          kind: opcode,
          xq: tokens[1][0],
        )
      of jgzxy:
        Instruction(
          kind: opcode,
          xjgzrov: tokens[1].toRegisterOrValue(),
          yjgzrov: tokens[2].toRegisterOrValue(),
        )
      else:
        Instruction(
          kind: opcode,
          x: tokens[1][0],
          yrov: tokens[2].toRegisterOrValue()
        )
    instructions &= instruction

  var queue0 = new seq[int]
  var queue1 = new seq[int]
  var prog0 = newProgram(instructions, 0, queue0, queue1)
  let s1 = block:
    prog0.exec()
    queue1[^1]

  var prog1 = newProgram(instructions, 1, queue1, queue0)
  while queue0[].len() != 0 or queue1[].len() != 0:
    if queue0[].len() != 0: prog0.exec()
    if queue1[].len() != 0: prog1.exec()

  Solution(s1: $s1, s2: $prog1.sends)
