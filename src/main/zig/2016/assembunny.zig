const std = @import("std");
const Assembunny = @This();
const Register = u2;

const Operand = union(enum) {
    Immediate: i8,
    Register: Register,

    fn detect(str: []const u8) !Operand {
        return if (str[0] <= '9')
            Operand{ .Immediate = try std.fmt.parseInt(i8, str, 10) }
        else
            Operand{ .Register = convertRegister(str) }
        ;
    }

    fn fetch(self: Operand, registers: []isize) isize {
        return switch (self) {
            .Immediate => |i| i,
            .Register => |r| registers[r],
        };
    }
};

const DualArgument = struct {
    op1: Operand, op2: Operand
};

const Instruction = union(enum) {
    Copy: DualArgument,
    Increment: Operand,
    Decrement: Operand,
    JumpNotZero: DualArgument,
    Toggle: Operand,
};

const Instructions = std.ArrayList(Instruction);

registers: [4]isize,
instructions: Instructions,

pub fn init(allocator: *std.mem.Allocator) Assembunny {
    var computer: Assembunny = undefined;
    _ = computer.reset();
    computer.instructions = Instructions.init(allocator);
    return computer;
}

pub fn deinit(self: *Assembunny) void {
    self.instructions.deinit();
}

pub fn reset(self: *Assembunny) *Assembunny {
    self.registers = [_]isize {0} ** 4;
    return self;
}

pub fn feed(self: *Assembunny, line: []const u8) !void {
    var tokens = std.mem.tokenize(line, " ");
        const opcode = tokens.next().?;
        const x = tokens.next().?;
        const y = tokens.next();

        const instruction = switch (opcode[0]) {
            'c' => Instruction{ .Copy = .{ .op1 = try Operand.detect(x), .op2 = try Operand.detect(y.?) } },
            'i' => Instruction{ .Increment = try Operand.detect(x) },
            'd' => Instruction{ .Decrement = try Operand.detect(x) },
            'j' => Instruction{ .JumpNotZero = .{ .op1 = try Operand.detect(x), .op2 = try Operand.detect(y.?) } },
            't' => Instruction{ .Toggle = try Operand.detect(x) },
            else => unreachable
        };
        try self.instructions.append(instruction);
}

pub fn runAndFetchRegisterA(self: *Assembunny) !isize {
    var instructions = try self.instructions.allocator.dupe(Instruction, self.instructions.items);
    defer self.instructions.allocator.free(instructions);
    var pc: isize = 0;
    while (pc < instructions.len) : (pc += 1) {
        switch (instructions[@intCast(u7, pc)]) {
            .Copy => |c| switch (c.op2) {
                .Register => |dest| self.registers[dest] = c.op1.fetch(&self.registers),
                else => {}
            },
            .Increment => |op| switch (op) {
                .Register => |dest| self.registers[dest] += 1,
                else => {}
            },
            .Decrement => |op| switch (op) {
                .Register => |dest| self.registers[dest] -= 1,
                else => {}
            },
            .JumpNotZero => |j| if (j.op1.fetch(&self.registers) != 0) { pc += j.op2.fetch(&self.registers) - 1; },
            .Toggle => |op| blk: {
                const targetPcMaybe = pc + op.fetch(&self.registers);
                if (targetPcMaybe < 0 or targetPcMaybe >= instructions.len) {
                    break :blk;
                }
                const targetPc = @intCast(usize, targetPcMaybe);
                instructions[targetPc] = switch (instructions[targetPc]) {
                    .Copy => |c| Instruction{ .JumpNotZero = c },
                    .Increment => |i| Instruction{ .Decrement = i },
                    .Decrement => |d| Instruction{ .Increment = d },
                    .JumpNotZero => |j| Instruction{ .Copy = j },
                    .Toggle => |t| Instruction{ .Increment = t },
                };
            },
        }
    }
    return self.registers[convertRegister("a")];
}

pub inline fn setRegister(self: *Assembunny, reg: []const u8, value: isize) *Assembunny {
    self.registers[convertRegister(reg)] = value;
    return self;
}

inline fn convertRegister(str: []const u8) Register {
    return @intCast(Register, str[0] - 'a');
}
