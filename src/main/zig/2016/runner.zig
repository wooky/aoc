const aoc = @import("../aoc.zig");
const kotlin_bridge = @import("../kotlin_bridge.zig");
const nim = @import("../nim_bridge.zig");

pub fn run(problem: *aoc.Problem, day: u16) anyerror!aoc.Solution { // TODO remove anyerror from here
    return switch (day) {
        1 => @import("day01.zig").run(problem),
        2 => @import("day02.zig").run(problem),
        3 => @import("day03.zig").run(problem),
        4 => nim.run(problem, 2016, 4),
        5 => @import("day05.zig").run(problem),
        6 => @import("day06.zig").run(problem),
        7 => nim.run(problem, 2016, 7),
        8 => @import("day08.zig").run(problem),
        9 => nim.run(problem, 2016, 9),
        10 => nim.run(problem, 2016, 10),
        12 => @import("day12.zig").run(problem),
        13 => nim.run(problem, 2016, 13),
        14 => nim.run(problem, 2016, 14),
        15 => nim.run(problem, 2016, 15),
        16 => nim.run(problem, 2016, 16),
        17 => nim.run(problem, 2016, 17),
        18 => nim.run(problem, 2016, 18),
        19 => nim.run(problem, 2016, 19),
        20 => @import("day20.zig").run(problem),
        21 => @import("day21.zig").run(problem),
        23 => @import("day23.zig").run(problem),
        else => unreachable
    };
}
