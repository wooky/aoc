const aoc = @import("../aoc.zig");

pub fn run(problem: *aoc.Problem, day: u16) anyerror!aoc.Solution { // TODO remove anyerror from here
    return switch (day) {
        1 => @import("day01.zig").run(problem),
        2 => @import("day02.zig").run(problem),
        3 => @import("day03.zig").run(problem),
        5 => @import("day05.zig").run(problem),
        6 => @import("day06.zig").run(problem),
        8 => @import("day08.zig").run(problem),
        12 => @import("day12.zig").run(problem),
        20 => @import("day20.zig").run(problem),
        21 => @import("day21.zig").run(problem),
        23 => @import("day23.zig").run(problem),
        else => @panic("Invalid 2016 day"),
    };
}
