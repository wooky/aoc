const aoc = @import("../aoc.zig");
const std = @import("std");

const Keypad = struct {
    keys: []const u8,
    dim: u8,
    idx: u8,
    buf: [8]u8 = undefined,
    buf_size: u8 = 0,

    fn init(keys: []const u8, dim: u8, idx: u8) Keypad {
        return Keypad { .keys = keys, .dim = dim, .idx = idx };
    }

    fn feed(self: *Keypad, dir: u8) void {
        switch (dir) {
            'U' => self.up(),
            'D' => self.down(),
            'L' => self.left(),
            'R' => self.right(),
            else => unreachable
        }
    }

    fn punch(self: *Keypad) void {
        self.buf_size += 1;
        self.buf[self.buf_size - 1] = self.keys[self.idx];
    }

    fn done(self: *const Keypad) []const u8 {
        return self.buf[0..self.buf_size];
    }

    fn up(self: *Keypad) void {
        if (self.idx >= self.dim) {
            self.adjust(self.idx - self.dim);
        }
    }

    fn down(self: *Keypad) void {
        if (self.idx / self.dim < self.dim - 1) {
            self.adjust(self.idx + self.dim);
        }
    }

    fn left(self: *Keypad) void {
        if (self.idx % self.dim != 0) {
            self.adjust(self.idx - 1);
        }
    }

    fn right(self: *Keypad) void {
        if ((self.idx + 1) % self.dim != 0) {
            self.adjust(self.idx + 1);
        }
    }

    fn adjust(self: *Keypad, new_idx: u8) void {
        if (self.keys[new_idx] != ' ') {
            self.idx = new_idx;
        }
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var keypad1 = Keypad.init("123456789", 3, 4);
    var keypad2 = Keypad.init("  1   234 56789 ABC   D  ", 5, 10);
    while (problem.line()) |line| {
        for (line) |dir| {
            keypad1.feed(dir);
            keypad2.feed(dir);
        }
        keypad1.punch();
        keypad2.punch();
    }

    return problem.solution(keypad1.done(), keypad2.done());
}
