pub const Intcode = @import("intcode.zig");
const aoc = @import("../aoc.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
    return try switch (day) {
        1 => @import("day01.zig").run(problem),
        2 => @import("day02.zig").run(problem),
        5 => @import("day05.zig").run(problem),
        7 => @import("day07.zig").run(problem),
        9 => @import("day09.zig").run(problem),
        11 => @import("day11.zig").run(problem),
        13 => @import("day13.zig").run(problem),
        else => unreachable,
    };
}
