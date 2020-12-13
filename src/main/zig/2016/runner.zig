const aoc = @import("../aoc.zig");
const kotlin_bridge = @import("../kotlin_bridge.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
    return switch (day) {
        4 => kotlin_bridge.run("2016", "04", problem),
        else => unreachable
    };
}
