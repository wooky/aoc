const aoc = @import("../aoc.zig");
const std = @import("std");
const Address = isize;

const SubInstruction = enum { ACC, JMP, NOP };
const Instruction = struct { instr: SubInstruction, offset: i16 };

const ExecutionResult = struct { pc: Address, acc: isize };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var instructions = std.ArrayList(Instruction).init(problem.allocator);
    defer instructions.deinit();
    while (problem.line()) |line| {
        const instr_str = line[0..3];
        const instr =
            if (std.mem.eql(u8, instr_str, "acc")) SubInstruction.ACC else if (std.mem.eql(u8, instr_str, "jmp")) SubInstruction.JMP else SubInstruction.NOP;
        const offset = (if (line[4] == '+') @as(i16, @intCast(1)) else -1) * try std.fmt.parseInt(i16, line[5..], 10);
        try instructions.append(.{ .instr = instr, .offset = offset });
    }

    const res1 = blk: {
        const result = try executeInstructions(instructions.items, problem.allocator);
        break :blk @as(usize, @intCast(result.acc));
    };

    const res2 = blk: {
        for (instructions.items, 0..) |instruction, i| {
            const old_instr = instruction.instr;
            const new_instr = switch (old_instr) {
                .ACC => continue,
                .JMP => SubInstruction.NOP,
                .NOP => SubInstruction.JMP,
            };
            instructions.items[i].instr = new_instr;
            const result = try executeInstructions(instructions.items, problem.allocator);
            if (result.pc == instructions.items.len) {
                break :blk @as(usize, @intCast(result.acc));
            }
            instructions.items[i].instr = old_instr;
        }
        unreachable;
    };

    return problem.solution(res1, res2);
}

fn executeInstructions(instructions: []const Instruction, allocator: std.mem.Allocator) !ExecutionResult {
    var visited = std.AutoHashMap(Address, void).init(allocator);
    defer visited.deinit();
    var pc: Address = 0;
    var acc: isize = 0;
    while (!visited.contains(pc) and pc != instructions.len) {
        try visited.put(pc, {});
        const instruction = instructions[@as(usize, @intCast(pc))];
        switch (instruction.instr) {
            .ACC => {
                acc += instruction.offset;
                pc += 1;
            },
            .JMP => pc += instruction.offset,
            .NOP => pc += 1,
        }
    }
    return ExecutionResult{ .pc = pc, .acc = acc };
}
