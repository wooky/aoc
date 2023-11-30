const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var res1: usize = 0;
    var res2: usize = 0;
    while (problem.line()) |line| {
        const mass = try std.fmt.parseInt(usize, line, 10);
        var fuel = getFuel(mass);
        res1 += fuel;
        while (fuel != 0) : (fuel = getFuel(fuel)) {
            res2 += fuel;
        }
    }
    return problem.solution(res1, res2);
}

fn getFuel(mass: usize) usize {
    return @max(mass / 3, 2) - 2;
}
