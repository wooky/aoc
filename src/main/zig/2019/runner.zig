const aoc = @import("../aoc.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
    return try switch (day) {
        2 => @import("day02.zig").run(problem),
        else => unreachable,
    };
}
