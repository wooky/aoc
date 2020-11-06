const aoc = @import("../aoc.zig");
const std = @import("std");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    intcode.input = 1;
    const res1 = blk: {
        while (true) {
            const diagnostic = (try intcode.run()).?;
            if (diagnostic != 0) {
                break :blk diagnostic;
            }
        }
    };

    intcode.reset();
    intcode.input = 5;
    const res2 = (try intcode.run()).?;

    return aoc.Solution{ .p1 = @intCast(usize, res1), .p2 = @intCast(usize, res2) };
}
