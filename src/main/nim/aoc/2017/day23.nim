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
    setxy = "set",
    subxy = "sub",
    mulxy = "mul",
    jnzxy = "jnz",
  Instruction = ref InstructionObj
  InstructionObj = object
    case kind: InstructionKind
    of jnzxy:
      xjnzrov: RegisterOrValue
      yjnzrov: RegisterOrValue
    else:
      x: char
      yrov: RegisterOrValue

  Program = object
    instructions: Instructions
    registers: RegisterTable
    pc: int
    mults: int

proc toRegisterOrValue(s: string): RegisterOrValue =
  if s[0] in 'a' .. 'z': RegisterOrValue(kind: reg, reg: s[0])
  else: RegisterOrValue(kind: val, val: s.parseInt())

proc value(rov: RegisterOrValue, registers: RegisterTable): int =
  case rov.kind
  of reg: registers.getOrDefault(rov.reg, 0)
  of val: rov.val

proc newProgram(
  instructions: Instructions,
): Program =
  Program(
    instructions: instructions,
    registers: initTable[char, int](),
    pc: 0,
    mults: 0,
  )

proc exec(program: var Program) =
  while program.pc in 0 ..< program.instructions.len():
    let instruction = program.instructions[program.pc]
    case instruction.kind
    of setxy:
      program.registers[instruction.x] = instruction.yrov.value(program.registers)
    of subxy:
      program.registers.mgetOrPut(instruction.x, 0) -= instruction.yrov.value(program.registers)
    of mulxy:
      program.registers.mgetOrPut(instruction.x, 0) *= instruction.yrov.value(program.registers)
      program.mults += 1
    of jnzxy:
      if instruction.xjnzrov.value(program.registers) != 0:
        program.pc += instruction.yjnzrov.value(program.registers) - 1
    
    program.pc += 1

proc day23*(file: string): Solution =
  var instructions = newSeq[Instruction]()
  for line in file.splitLines():
    if line == "":
      continue
    let tokens = line.splitWhitespace()
    let opcode = parseEnum[InstructionKind](tokens[0])
    let instruction =
      case opcode
      of jnzxy:
        Instruction(
          kind: opcode,
          xjnzrov: tokens[1].toRegisterOrValue(),
          yjnzrov: tokens[2].toRegisterOrValue(),
        )
      else:
        Instruction(
          kind: opcode,
          x: tokens[1][0],
          yrov: tokens[2].toRegisterOrValue()
        )
    instructions &= instruction

  let s1 = block:
    var prog = newProgram(instructions)
    prog.exec()
    prog.mults

  # through the power of reverse engineering the damn thing - see day23.ods
  let s2 = block:
    var nonPrimes = 0
    for i in countup(109300, 126300, 17):
      block attempt:
        for j in 2 .. i/%2:
          if i %% j == 0:
            nonPrimes += 1
            break attempt
    nonPrimes

  Solution(s1: $s1, s2: $s2)
