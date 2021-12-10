const aoc = @import("../aoc.zig");
const std = @import("std");
const Register = u2;

const Instruction = union(enum) {
    CopyInteger: struct { int: u8, dest: Register },
    CopyRegister: struct { reg: u8, dest: Register },
    Increment: u8,
    Decrement: u8,
    Jump: i8,
    JumpNotZero: struct { reg: u8, offset: i8 }
};

const Computer = struct {
    const Instructions = std.ArrayList(Instruction);

    registers: [4]usize,
    pc: i8,
    instructions: Instructions,

    fn init(allocator: *std.mem.Allocator) Computer {
        var computer: Computer = undefined;
        computer.reset();
        computer.instructions = Instructions.init(allocator);
        return computer;
    }

    fn deinit(self: *Computer) void {
        self.instructions.deinit();
    }

    fn reset(self: *Computer) void {
        self.registers = [_]usize {0} ** 4;
        self.pc = 0;
    }

    fn run(self: *Computer) void {
        while (self.pc < self.instructions.items.len) : (self.pc += 1) {
            switch (self.instructions.items[@intCast(u7, self.pc)]) {
                .CopyInteger => |ci| self.registers[ci.dest] = ci.int,
                .CopyRegister => |cr| self.registers[cr.dest] = self.registers[cr.reg],
                .Increment => |dest| self.registers[dest] += 1,
                .Decrement => |dest| self.registers[dest] -= 1,
                .Jump => |offset| self.pc += offset - 1,
                .JumpNotZero => |jnz| if (self.registers[jnz.reg] != 0) { self.pc += jnz.offset - 1; },
            }
        }
    }

    inline fn fetchRegisterA(self: *const Computer) usize {
        return self.registers[convertRegister("a")];
    }

    inline fn convertRegister(str: []const u8) Register {
        return @intCast(Register, str[0] - 'a');
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var computer = Computer.init(problem.allocator);
    defer computer.deinit();
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, " ");
        const opcode = tokens.next().?;
        const x = tokens.next().?;
        const y = tokens.next();

        const instruction = switch (opcode[0]) {
            'c' =>
                if (x[0] <= '9')
                    Instruction{ .CopyInteger = .{ .int = try std.fmt.parseInt(u8, x, 10), .dest = Computer.convertRegister(y.?) } }
                else
                    Instruction{ .CopyRegister = .{ .reg = Computer.convertRegister(x), .dest = Computer.convertRegister(y.?) } }
                ,
            'i' => Instruction{ .Increment = Computer.convertRegister(x) },
            'd' => Instruction{ .Decrement = Computer.convertRegister(x) },
            'j' =>
                if (x[0] <= '9')
                    Instruction{ .Jump = try std.fmt.parseInt(i8, y.?, 10) }
                else
                    Instruction{ .JumpNotZero = .{ .reg = Computer.convertRegister(x), .offset = try std.fmt.parseInt(i8, y.?, 10) } },
            else => unreachable
        };
        try computer.instructions.append(instruction);
    }

    computer.run();
    const s1 = computer.fetchRegisterA();

    computer.reset();
    computer.registers[Computer.convertRegister("c")] = 1;
    computer.run();
    const s2 = computer.fetchRegisterA();

    return problem.solution(s1, s2);
}
