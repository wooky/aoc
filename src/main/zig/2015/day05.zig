const aoc = @import("../aoc.zig");
const std = @import("std");

const Part1 = struct {
    vowels: u8 = 0,
    last: u8 = 0,
    has_double: bool = false,
    dead: bool = false,

    fn feed(self: *Part1, c: u8) void {
        if (self.dead) {
            return;
        }
        switch (c) {
            'a', 'e', 'i', 'o', 'u' => self.vowels += 1,
            'b' => if (self.last == 'a') {self.dead = true; return;},
            'd' => if (self.last == 'c') {self.dead = true; return;},
            'q' => if (self.last == 'p') {self.dead = true; return;},
            'y' => if (self.last == 'x') {self.dead = true; return;},
            else => {},
        }
        if (c == self.last) {
            self.has_double = true;
        }
        self.last = c;
    }

    fn eval(self: *Part1) u16 {
        return if (!self.dead and self.vowels >= 3 and self.has_double) 1 else 0;
    }
};

const Part2 = struct {
    const LetterPairs = std.AutoHashMap([2]u8, usize);

    last0: u8 = 0,
    last1: u8 = 0,
    saw_pair: bool = false,
    saw_repeat: bool = false,
    pairs: LetterPairs,

    fn init(allocator: *std.mem.Allocator) Part2 {
        return Part2 { .pairs = LetterPairs.init(allocator) };
    }

    fn deinit(self: *Part2) void {
        self.pairs.deinit();
    }

    fn feed(self: *Part2, c: u8, idx: usize) !void {
        if (c == self.last0) {
            self.saw_repeat = true;
        }
        if (!self.saw_pair) {
            const pair = [2]u8{ self.last1, c };
            if (self.pairs.get(pair)) |inserted_idx| {
                if (idx - inserted_idx > 1) {
                    self.saw_pair = true;
                }
            }
            else {
                _ = try self.pairs.put(pair, idx);
            }
        }
        self.last0 = self.last1;
        self.last1 = c;
    }

    fn eval(self: *Part2) u16 {
        return if (self.saw_pair and self.saw_repeat) 1 else 0;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var nice1: u16 = 0;
    var nice2: u16 = 0;

    while (problem.line()) |line| {
        var part1 = Part1 {};
        var part2 = Part2.init(problem.allocator);
        defer part2.deinit();
        for (line) |c, idx| {
            part1.feed(c);
            try part2.feed(c, idx);
        }
        nice1 += part1.eval();
        nice2 += part2.eval();
    }

    return aoc.Solution{ .p1 = nice1, .p2 = nice2 };
}
