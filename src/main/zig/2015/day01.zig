const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) aoc.Solution {
    var floor: i16 = 0;
    var basement_idx: usize = 0;
    for (problem.input, 0..) |c, idx| {
        floor += switch (c) {
            '(' => @as(i16, @intCast(1)),
            ')' => -1,
            else => unreachable,
        };
        if (floor == -1 and basement_idx == 0) {
            basement_idx = idx + 1;
        }
    }
    return problem.solution(floor, basement_idx);
}
