const aoc = @import("../aoc.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
    return try switch (day) {
        1 => @import("day01.zig").run(problem),
        2 => @import("day02.zig").run(problem),
        3 => @import("day03.zig").run(problem),
        4 => @import("day04.zig").run(problem),
        5 => @import("day05.zig").run(problem),
        6 => @import("day06.zig").run(problem),
        7 => @import("day07.zig").run(problem),
        8 => @import("day08.zig").run(problem),
        9 => @import("day09.zig").run(problem),
        10 => @import("day10.zig").run(problem),
        11 => @import("day11.zig").run(problem),
        12 => @import("day12.zig").run(problem),
        13 => @import("day13.zig").run(problem),
        14 => @import("day14.zig").run(problem),
        15 => @import("day15.zig").run(problem),
        16 => @import("day16.zig").run(problem),
        17 => @import("day17.zig").run(problem),
        18 => @import("day18.zig").run(problem),
        19 => @import("day19.zig").run(problem),
        20 => @import("day20.zig").run(problem),
        22 => @import("day22.zig").run(problem),
        else => unreachable,
    };
}
