const aoc = @import("../aoc.zig");
const kotlin_bridge = @import("../kotlin_bridge.zig");

pub fn run(problem: *aoc.Problem, day: u16) anyerror!aoc.Solution { // TODO remove anyerror from here
    return switch (day) {
        1 => @import("day01.zig").run(problem),
        2 => @import("day02.zig").run(problem),
        3 => @import("day03.zig").run(problem),
        4 => kotlin_bridge.run("2016", "04", problem),
        5 => @import("day05.zig").run(problem),
        6 => @import("day06.zig").run(problem),
        7 => kotlin_bridge.run("2016", "07", problem),
        8 => @import("day08.zig").run(problem),
        9 => kotlin_bridge.run("2016", "09", problem),
        10 => kotlin_bridge.run("2016", "10", problem),
        12 => @import("day12.zig").run(problem),
        13 => kotlin_bridge.run("2016", "13", problem),
        14 => kotlin_bridge.run("2016", "14", problem),
        15 => kotlin_bridge.run("2016", "15", problem),
        16 => kotlin_bridge.run("2016", "16", problem),
        17 => kotlin_bridge.run("2016", "17", problem),
        18 => kotlin_bridge.run("2016", "18", problem),
        19 => kotlin_bridge.run("2016", "19", problem),
        20 => @import("day20.zig").run(problem),
        else => unreachable
    };
}
