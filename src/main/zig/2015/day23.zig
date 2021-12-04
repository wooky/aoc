const aoc = @import("../aoc.zig");
const std = @import("std");

const Instruction = [3][]const u8;

const Computer = struct {
    const Registers = std.StringHashMap(usize);
    const Instructions = std.StringHashMap(fn(*Computer, Instruction)std.fmt.ParseIntError!isize);

    registers: Registers,
    instructions: Instructions,

    fn init(allocator: *std.mem.Allocator) !Computer {
        var computer = Computer { .registers = Registers.init(allocator), .instructions = Instructions.init(allocator) };

        try computer.instructions.putNoClobber("hlf", hlf);
        try computer.instructions.putNoClobber("tpl", tpl);
        try computer.instructions.putNoClobber("inc", inc);
        try computer.instructions.putNoClobber("jmp", jmp);
        try computer.instructions.putNoClobber("jie", jie);
        try computer.instructions.putNoClobber("jio", jio);

        return computer;
    }

    fn deinit(self: *Computer) void {
        self.registers.deinit();
        self.instructions.deinit();
    }

    fn execute(self: *Computer, instructions: []const Instruction, a: usize) !usize {
        try self.registers.put("a", a);
        try self.registers.put("b", 0);

        var pc: isize = 0;
        while (pc >= 0 and pc < instructions.len) {
            const instruction = instructions[@intCast(usize, pc)];
            const callback = self.instructions.get(instruction[0]).?;
            pc += try callback(self, instruction);
        }

        return self.registers.get("b").?;
    }

    fn hlf(self: *Computer, instruction: Instruction) std.fmt.ParseIntError!isize {
        self.registers.getPtr(instruction[1]).?.* /= 2;
        return 1;
    }

    fn tpl(self: *Computer, instruction: Instruction) std.fmt.ParseIntError!isize {
        self.registers.getPtr(instruction[1]).?.* *= 3;
        return 1;
    }

    fn inc(self: *Computer, instruction: Instruction) std.fmt.ParseIntError!isize {
        self.registers.getPtr(instruction[1]).?.* += 1;
        return 1;
    }

    fn jmp(self: *Computer, instruction: Instruction) std.fmt.ParseIntError!isize {
        return self.jump(instruction[1]);
    }

    fn jie(self: *Computer, instruction: Instruction) std.fmt.ParseIntError!isize {
        return if (self.registers.get(instruction[1]).? % 2 == 0) self.jump(instruction[2]) else 1;
    }

    fn jio(self: *Computer, instruction: Instruction) std.fmt.ParseIntError!isize {
        return if (self.registers.get(instruction[1]).? == 1) self.jump(instruction[2]) else 1;
    }

    fn jump(self: *Computer, offset: []const u8) std.fmt.ParseIntError!isize {
        const dir: isize = if (offset[0] == '+') 1 else -1;
        return dir * try std.fmt.parseInt(isize, offset[1..], 10);
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var computer = try Computer.init(problem.allocator);
    defer computer.deinit();

    var instructions = std.ArrayList(Instruction).init(problem.allocator);
    defer instructions.deinit();
    while (problem.line()) |line| {
        var instruction: Instruction = undefined;
        var idx: usize = 0;
        var tokens = std.mem.tokenize(line, " ,");
        while (tokens.next()) |token| {
            instruction[idx] = token;
            idx += 1;
        }
        try instructions.append(instruction);
    }

    return problem.solution(
        try computer.execute(instructions.items, 0),
        try computer.execute(instructions.items, 1)
    );
}