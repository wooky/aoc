const aoc = @import("../aoc.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
    return try switch (day) {
        1 => @import("day01.zig").run(problem),
        2 => @import("day02.zig").run(problem),
        3 => @import("day03.zig").run(problem),
        4 => @import("day04.zig").run(problem),
        5 => @import("day05.zig").run(problem),
        6 => @import("day06.zig").run(problem),
        else => unreachable,
    };
}
