const aoc = @import("../aoc.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
    return try switch (day) {
        1 => @import("day01.zig").run(problem),
        else => unreachable,
    };
}
