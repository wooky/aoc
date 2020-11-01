const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) void {
    var floor: i16 = 0;
    var basement_idx: usize = 0;
    for (problem.input) |c, idx| {
        floor += switch (c) {
            '(' => @intCast(i16, 1),
            ')' => -1,
            else => unreachable,
        };
        if (floor == -1 and basement_idx == 0) {
            basement_idx = idx + 1;
        }
    }
    std.debug.warn("{}\n{}\n", .{floor, basement_idx});
}
