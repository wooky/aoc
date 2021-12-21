const aoc = @import("../aoc.zig");
const std = @import("std");

const MemoryGame = struct {
    const NumbersTurns = std.AutoHashMap(usize, usize);

    numbers_turns: NumbersTurns,
    turn: usize = 1,
    last_spoken: usize = undefined,

    fn init(allocator: std.mem.Allocator) MemoryGame {
        return MemoryGame { .numbers_turns = NumbersTurns.init(allocator) };
    }

    fn deinit(self: *MemoryGame) void {
        self.numbers_turns.deinit();
    }

    fn append(self: *MemoryGame, number: usize) !void {
        try self.numbers_turns.put(number, self.turn);
        self.turn += 1;
    }

    fn fetchTargetNumber(self: *MemoryGame, target: usize) !usize {
        while (self.turn <= target) : (self.turn += 1) {
            if (self.numbers_turns.getEntry(self.last_spoken)) |kv| {
                self.last_spoken = self.turn - 1 - kv.value_ptr.*;
                kv.value_ptr.* = self.turn - 1;
            }
            else {
                try self.numbers_turns.putNoClobber(self.last_spoken, self.turn - 1);
                self.last_spoken = 0;
            }
        }
        return self.last_spoken;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var memory_game = MemoryGame.init(problem.allocator);
    defer memory_game.deinit();

    var tokens = std.mem.tokenize(u8, problem.input, ",\n");
    while (tokens.next()) |number| {
        try memory_game.append(try std.fmt.parseInt(usize, number, 10));
    }

    return problem.solution(
        try memory_game.fetchTargetNumber(2020),
        try memory_game.fetchTargetNumber(30000000)
    );
}
