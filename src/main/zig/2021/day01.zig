const aoc = @import("../aoc.zig");
const std = @import("std");

const Window = struct {
    size: u8,
    last_sum: u16 = 0,
    increases: u16 = 0,

    fn init(size: u8) Window {
        return .{ .size = size };
    }

    fn consume(self: *Window, numbers: []const u16) void {
        if (numbers.len <= self.size) {
            self.last_sum += numbers[numbers.len - 1];
            return;
        }

        var sum: u16 = 0;
        var idx = numbers.len - self.size;
        while (idx < numbers.len) : (idx += 1) {
            sum += numbers[idx];
        }

        if (sum > self.last_sum) {
            self.increases += 1;
        }
        self.last_sum = sum;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var depths = std.ArrayList(u16).init(problem.allocator);
    defer depths.deinit();
    var window1 = Window.init(1);
    var window3 = Window.init(3);
    while (problem.line()) |line| {
        var depth = try std.fmt.parseInt(u16, line, 10);
        try depths.append(depth);
        window1.consume(depths.items);
        window3.consume(depths.items);
    }

    return aoc.Solution {
        .p1 = window1.increases,
        .p2 = window3.increases,
    };
}
